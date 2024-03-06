{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Collection.Generate where

import Codec.Archive.Zip
import Collection.Utils (genNoteGuid, removeIfExists)
import Control.Monad.Cont (MonadIO (liftIO))
import Control.Monad.State (StateT (runStateT), gets)
import Data.Aeson (decode, decodeStrictText, encode)
import Data.Aeson.Key qualified as AK (fromString, toString)
import Data.Aeson.KeyMap qualified as AKM (fromList, insert, keys)
import Data.Aeson.Text (encodeToLazyText)
import Data.Bifunctor (second)
import Data.ByteString.Lazy qualified as BL
import Data.ByteString qualified as BS
import Data.Char (chr)
import Data.FileEmbed (embedDir, embedFile)
import Data.Functor (($>))
import Data.Maybe (fromJust, fromMaybe)
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Encoding qualified as TE
import Data.Time.Clock.POSIX
import Database.SQLite.Simple
import System.FilePath ((</>))
import Types (DeckGenInfo (..), MediaDeck, MediaItem (DeckMedia), PankyDeck, RenderedCard (RCard), RenderedDeck, PankyApp)
import Types.Anki.JSON (Deck (..), Decks, MConf (..), Model (..), Models)
import Types.Anki.SQL as ANS
import System.IO (withBinaryFile, IOMode (ReadMode))
import Types.CLI (PankyConfig(outputDirPConf))

addCard :: Int -> Connection -> RenderedCard -> PankyDeck ()
addCard modelId conn (RCard front back tags) = do
  noteGUID <- gets ((\dn -> genNoteGuid (T.unpack dn) (T.unpack front) []) . deckName)

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
          tagsNote = T.pack $ show tags,
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
  let dataFiles = map (second BL.fromStrict) $(embedDir "data")
  let queries = TE.decodeUtf8 $ fromJust $ lookup "setup-migrations.sql" dataFiles
  let queryString = case reverse $ T.splitOn ";" $ T.replace "\n" "" queries of
        [] -> error "No queries found to run setup migrations"
        ("" : xs) -> reverse xs
        commands -> reverse commands
  mapM_ (execute_ conn . Query . T.toStrict) queryString

  let colMConfDefault = fromJust $ (decode (fromJust $ lookup "default-anki-json/conf.json" dataFiles) :: Maybe MConf)
  let colModelDefault = fromJust $ (decode (fromJust $ lookup "default-anki-json/models.json" dataFiles) :: Maybe Model)
  let colDConf = TE.decodeUtf8 $ fromJust $ lookup "default-anki-json/dconf.json" dataFiles
  let cssDefault = TE.decodeUtf8 $ fromJust $ lookup "css/card.css" dataFiles
  let latexPre = TE.decodeUtf8 $ fromJust $ lookup "latex/preamble.tex" dataFiles
  let latexPost = TE.decodeUtf8 $ fromJust $ lookup "latex/postamble.tex" dataFiles

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

generateMediaEntry :: (Int, MediaItem) -> IO Entry
generateMediaEntry (idx, DeckMedia url _) =
  withBinaryFile url ReadMode $ \h ->
    BS.hGetContents h >>= return . BL.fromStrict
    >>= (\x -> toEntry (show idx) . round <$> getPOSIXTime <*> pure x)

writeDbToApkg :: MediaDeck -> T.Text -> FilePath -> PankyApp ()
writeDbToApkg media colName dbpath = do
  let archivePath = "collection.anki2"
  mentry <- liftIO $ mapM generateMediaEntry media
  let mediajson = encode $ AKM.fromList (map (\(inx, DeckMedia _ internalRep) -> (AK.fromString (show inx), internalRep)) media)
  mediajsonFile <- liftIO $ toEntry "media" . round <$> getPOSIXTime <*> pure mediajson
  cardDB <- liftIO $ toEntry archivePath . round <$> getPOSIXTime <*> BL.readFile dbpath
  let archive = foldl (flip addEntryToArchive) emptyArchive (mediajsonFile : cardDB : mentry)
      archiveName = colName <> ".apkg"
  outputDir <- gets outputDirPConf
  liftIO $ BL.writeFile (outputDir </> T.unpack archiveName) $ fromArchive archive

createCollectionDb :: FilePath -> IO Connection
createCollectionDb dbpath = removeIfExists dbpath *> open dbpath

addCardsToDeck :: Connection -> [Int] -> RenderedDeck -> PankyDeck ()
addCardsToDeck c modelKeys = mapM_ (addCard (head modelKeys) c)

generateDeck :: Connection -> [Int] -> RenderedDeck -> DeckGenInfo -> IO ()
generateDeck conn modelKeys renderedDeck genInfo = do
  miliEpoc :: Int <- floor . (* 10000) <$> getPOSIXTime
  let colDeckDefaultRaw = BL.fromStrict $(embedFile "data/default-anki-json/deck.json")
  let colDeckDefault = fromMaybe (error "Could not read default deck.json file") $ decode colDeckDefaultRaw
  let newDeck =
        colDeckDefault
          { confDeck = Just 1,
            idDeck = Just (deckId genInfo),
            modDeck = Just miliEpoc,
            nameDeck = Just (deckName genInfo)
          }

  putStrLn $ "Rendering deck: " ++ show (deckName genInfo)
  [[decksResult :: T.Text]] <- query_ conn (Query "SELECT decks FROM col")
  let decks :: Decks =
        fromMaybe
          (error "Could not read decks field from collection DB table")
          (decodeStrictText $ T.toStrict decksResult)
      appendedDecks = AKM.insert (AK.fromString (show (deckId genInfo))) newDeck decks
  execute conn (Query "UPDATE col SET decks = ?") (Only $ encodeToLazyText appendedDecks)
  runStateT (addCardsToDeck conn modelKeys renderedDeck) genInfo $> ()
