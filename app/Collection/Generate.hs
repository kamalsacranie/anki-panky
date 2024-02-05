{-# LANGUAGE OverloadedStrings #-}

module Collection.Generate where

import Codec.Archive.Zip
import Collection.Utils
import Control.Monad.Cont (MonadIO (liftIO), foldM, foldM_)
import Control.Monad.State (StateT (runStateT), gets, modify)
import Data.Aeson (decodeFileStrict)
import Data.Aeson.Key
import Data.Aeson.KeyMap (fromList, keys)
import Data.Aeson.Text (encodeToLazyText)
import Data.ByteString.Lazy qualified as BS
import Data.Char (chr)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as IOT
import Data.Text.Lazy qualified as TL
import Data.Time.Clock.POSIX
import Database.SQLite.Simple
import System.Random
import Types.Anki as A

addCard :: Int -> Int -> Connection -> T.Text -> T.Text -> IO ()
addCard dId modelId conn front back = do
  gen <- getStdGen
  let noteGUID :: Int
      noteGUID = fst $ random gen

  -- noteId <- round . (* 1000) <$> getPOSIXTime :: IO Int
  noteId <- randomRIO (1, 1000)
  execute
    conn
    "INSERT INTO notes VALUES(?,?,?,?,?,?,?,?,?,?,?)"
    A.Note
      { idNote = noteId,
        guidNote = T.pack $ show noteGUID,
        midNote = modelId,
        modNote = noteId,
        usnNote = -1,
        tagsNote = "",
        fldsNote = front <> T.singleton (chr 0x1f) <> back <> T.singleton (chr 0x1f) <> T.pack (show noteGUID),
        sfldNote = front,
        csumNote = 0,
        flagsNote = 0,
        dataNote = ""
      }

  -- cardId <- ((+ 1) . round . (* 1000) <$> getPOSIXTime) :: IO Int
  cardId <- randomRIO (1, 1000)
  execute
    conn
    "INSERT INTO cards VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
    A.Card
      { idCard = cardId,
        nidCard = noteId,
        didCard = dId,
        ordCard = 0,
        modCard = cardId, -- time the card was modified
        usnCard = -1,
        typeCard = 0,
        queueCard = 0,
        dueCard = 0,
        ivlCard = 0,
        factorCard = 0,
        repsCard = 0,
        lapsesCard = 0,
        leftCard = 0,
        odueCard = 0,
        odidCard = 0,
        flagsCard = 0,
        dataCard = ""
      }

setupCollectionDb :: Connection -> Panky [Int]
setupCollectionDb conn = do
  queries <- liftIO $ IOT.readFile "./app/setup-migrations.sql"
  let queryString = case reverse $ T.splitOn ";" $ T.replace "\n" "" queries of
        [] -> error "No queries found to run setup migrations"
        ("" : xs) -> reverse xs
        commands -> reverse commands
  mapM_ (liftIO . (execute_ conn . Query)) queryString
  colConfDefault <- liftIO $ fromJust <$> (decodeFileStrict "./app/defaultjson/conf.json" :: IO (Maybe Conf))
  colModelDefault <- liftIO $ fromJust <$> (decodeFileStrict "./app/defaultjson/models.json" :: IO (Maybe Model))
  colDeckDefault <- liftIO $ fromJust <$> (decodeFileStrict "./app/defaultjson/deck.json" :: IO (Maybe Deck))
  colDConf <- liftIO $ IOT.readFile "./app/defaultjson/dconf.json"
  cssDefault <- liftIO $ IOT.readFile "./app/defaultjson/card.css"
  latexPre <- liftIO $ IOT.readFile "./app/defaultjson/preamble.tex"
  latexPost <- liftIO $ IOT.readFile "./app/defaultjson/postamble.tex"

  currTime <- liftIO getPOSIXTime
  let miliEpoc = floor $ currTime * 1000 :: Int
      secEpoc = floor currTime :: Int

  modify $ \s -> s {deckId = Just miliEpoc}
  dId <- gets (fromMaybe (error "No Deck Id set during generation.") . deckId)
  dName <- gets deckName
  -- Keeping at one now as this is for the default deck. But i'm not sure what
  -- that meanas. Perhaps we should not have the default deck at all?
  let colDecks =
        fromList
          [ ( fromText $ T.pack (show dId),
              colDeckDefault
                { confDeck = Just 1,
                  idDeck = Just dId,
                  modDeck = Just miliEpoc,
                  nameDeck = Just dName
                }
            )
          ] ::
          Decks

  -- TODO: Handle multiple models
  let modelId = T.pack $ show miliEpoc
  let colModels =
        fromList
          [ ( fromText modelId,
              colModelDefault
                { cssModel = Just cssDefault,
                  didModel = Just dId,
                  idModel = Just modelId,
                  latexPreModel = Just latexPre,
                  latexPostModel = Just latexPost,
                  modModel = Just secEpoc,
                  typeModel = Just 0
                }
            )
          ] ::
          Models

  let modelKeyTexts = toText <$> keys colModels
      modelKeys = read . T.unpack <$> modelKeyTexts :: [Int] -- couldn't get show to wrok here
  let colConf = colConfDefault {curModelConf = Just (last modelKeyTexts)}

  liftIO $
    execute
      conn
      (Query "INSERT INTO col VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?)")
      Col
        { idCol = 1, -- will always be one because i don't think its necessary to do multiple collecitons
          crtCol = secEpoc, -- created in seconds
          modCol = miliEpoc, -- modified in miliseconds
          scmCol = miliEpoc, -- schema modified in miliseconds
          verCol = 11, -- Version of Anki. It seems like 11 is the latest version? idk
          dtyCol = 0, -- All collections generated will be clean
          usnCol = 0,
          lsCol = 0, -- last sync time, not important for a new deck
          confCol = TL.toStrict $ encodeToLazyText colConf, -- config
          modelsCol = TL.toStrict $ encodeToLazyText colModels,
          decksCol = TL.toStrict $ encodeToLazyText colDecks,
          dconfCol = colDConf,
          tagsCol = "{}" -- todo, investigate how these tags are used (don't think there are any)
        }
  return modelKeys

dbPath :: FilePath
dbPath = "collection.anki2" -- required name for the anki db

writeDbToApkg :: DeckGenInfo -> IO ()
writeDbToApkg genInfo = do
  let archivePath = "collection.anki2"
  entry <-
    toEntry
      archivePath
      . round
      <$> getPOSIXTime
      <*> BS.readFile dbPath
  let archive = addEntryToArchive entry emptyArchive
      archiveName = deckFileName genInfo <> ".apkg"
  BS.writeFile archiveName $ fromArchive archive

createCollectionDb :: IO Connection
createCollectionDb = removeIfExists dbPath *> open dbPath

generateCollection :: DeckGenInfo -> [(T.Text, T.Text)] -> IO ()
generateCollection genInfo deck = do
  conn <- createCollectionDb
  (modelKeys, infoPostSetup) <- runStateT (setupCollectionDb conn) genInfo
  let ac (x, y) = addCard (last modelKeys) (head modelKeys) conn x y
  mapM_ ac deck
  liftIO $ close conn
  liftIO $ writeDbToApkg genInfo
