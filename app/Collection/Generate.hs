{-# LANGUAGE OverloadedStrings #-}

module Collection.Generate where

import Codec.Archive.Zip
import Collection.Utils (genNoteGuid, removeIfExists)
import Control.Monad.Cont (MonadIO (liftIO))
import Control.Monad.State (gets)
import Data.Aeson (decodeFileStrict, encode)
import Data.Aeson.Key qualified as AK (fromString, toString)
import Data.Aeson.KeyMap qualified as AKM (fromList, keys)
import Data.Aeson.Text (encodeToLazyText)
import Data.ByteString.Lazy qualified as BS
import Data.Char (chr)
import Data.Maybe (fromJust)
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.IO qualified as IOT
import Data.Time.Clock.POSIX
import Database.SQLite.Simple
import Types (DeckGenInfo (..), MediaDeck, MediaItem (DeckMedia), Panky)
import Types.Anki.JSON (MConf (..), Model (..), Models)
import Types.Anki.SQL as ANS

addCard :: Int -> Connection -> (T.Text, T.Text) -> Panky ()
addCard modelId conn (front, back) = do
  noteGUID <- gets ((\dn -> genNoteGuid (T.unpack dn) (T.unpack front) []) . deckName)
  liftIO $ putStrLn noteGUID

  -- technically these should be miliseconds but its not fast enought
  noteId <- liftIO $ floor . (* 10000) <$> getPOSIXTime
  liftIO $
    execute
      conn
      "INSERT INTO notes VALUES(?,?,?,?,?,?,?,?,?,?,?)"
      ANS.Note
        { idNote = noteId,
          guidNote = T.pack noteGUID,
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

  cardId <- liftIO $ floor . (* 10000) <$> getPOSIXTime
  dId <- gets deckId
  liftIO $
    execute
      conn
      "INSERT INTO cards VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
      ANS.Card
        { idCard = cardId,
          nidCard = noteId,
          didCard = dId,
          ordCard = 0,
          modCard = cardId `div` 10000, -- time the card was modified
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

setupCollectionDb :: Connection -> IO [Int]
setupCollectionDb conn = do
  queries <- IOT.readFile "./data/setup-migrations.sql"
  let queryString = case reverse $ T.splitOn ";" $ T.replace "\n" "" queries of
        [] -> error "No queries found to run setup migrations"
        ("" : xs) -> reverse xs
        commands -> reverse commands
  mapM_ (execute_ conn . Query . T.toStrict) queryString

  colMConfDefault <- fromJust <$> (decodeFileStrict "./data/default-anki-json/conf.json" :: IO (Maybe MConf))
  colModelDefault <- fromJust <$> (decodeFileStrict "./data/default-anki-json/models.json" :: IO (Maybe Model))
  colDConf <- IOT.readFile "./data/default-anki-json/dconf.json"
  cssDefault <- IOT.readFile "./data/css/card.css"
  latexPre <- IOT.readFile "./data/latex/preamble.tex"
  latexPost <- IOT.readFile "./data/latex/postamble.tex"

  currTime <- getPOSIXTime
  let miliEpoc = floor $ currTime * 1000 :: Int
      secEpoc = floor currTime :: Int

  let colModels =
        AKM.fromList
          [ ( AK.fromString $ T.unpack (idModel colModelDefault),
              colModelDefault
                { cssModel = Just cssDefault,
                  didModel = Nothing, -- update the did later
                  latexPreModel = Just latexPre,
                  latexPostModel = Just latexPost,
                  modModel = Just secEpoc,
                  typeModel = Just 0
                }
            )
          ] ::
          Models

  let modelKeyTexts = AK.toString <$> AKM.keys colModels
      modelKeys = read <$> modelKeyTexts :: [Int] -- couldn't get show to wrok here
  let colConf = colMConfDefault {curModelMConf = Just $ T.pack (last modelKeyTexts)}

  execute
    conn
    (Query "INSERT INTO col VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?)")
    ANS.Col
      { idCol = 1,
        crtCol = secEpoc,
        modCol = miliEpoc,
        scmCol = miliEpoc,
        verCol = 11,
        dtyCol = 0, -- All collections generated will be clean
        usnCol = 0,
        lsCol = 0, -- last sync time, not important for a new deck
        confCol = encodeToLazyText colConf, -- config
        modelsCol = encodeToLazyText colModels,
        decksCol = "{}",
        dconfCol = colDConf,
        tagsCol = "{}" -- todo, investigate how these tags are used (don't think there are any)
      }
  return modelKeys

dbPath :: FilePath
dbPath = "collection.anki2" -- required name for the anki db

generateMediaEntry :: (Int, MediaItem) -> IO Entry
generateMediaEntry (idx, DeckMedia url _) = toEntry (show idx) . round <$> getPOSIXTime <*> BS.readFile url

writeDbToApkg :: MediaDeck -> T.Text -> IO ()
writeDbToApkg media colName = do
  let archivePath = "collection.anki2"
  mentry <- mapM generateMediaEntry media
  let mediajson = encode $ AKM.fromList (map (\(inx, DeckMedia _ internalRep) -> (AK.fromString (show inx), internalRep)) media)
  mediajsonFile <- toEntry "media" . round <$> getPOSIXTime <*> pure mediajson
  cardDB <- toEntry archivePath . round <$> getPOSIXTime <*> BS.readFile dbPath
  let archive = foldl (flip addEntryToArchive) emptyArchive (mediajsonFile : cardDB : mentry)
      archiveName = colName <> ".apkg" -- should be the package name
  BS.writeFile (T.unpack archiveName) $ fromArchive archive

createCollectionDb :: IO Connection
createCollectionDb = removeIfExists dbPath *> open dbPath

generateDeck :: Connection -> [Int] -> [(T.Text, T.Text)] -> Panky ()
generateDeck c modelKeys = mapM_ (addCard (head modelKeys) c)
