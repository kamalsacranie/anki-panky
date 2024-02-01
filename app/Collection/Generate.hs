{-# LANGUAGE OverloadedStrings #-}

module Collection.Generate where

import Codec.Archive.Zip
import Collection.Utils
import Data.Aeson (decodeStrictText)
import Data.Aeson.Key (toText)
import Data.Aeson.KeyMap (keys)
import Data.ByteString.Lazy qualified as BS
import Data.Char (chr)
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Text.IO qualified as IOT
import Data.Time.Clock.POSIX
import Database.SQLite.Simple
import System.Random
import Types.Anki as A

addCard :: Int -> Connection -> T.Text -> T.Text -> IO ()
addCard modelId conn front back = do
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
        didCard = 1288033183,
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

setupCollectionDb :: Connection -> IO [Int]
setupCollectionDb conn = do
  queries <- IOT.readFile "./app/setup-migrations.sql"
  let queryString = case reverse $ T.splitOn ";" $ T.replace "\n" "" queries of
        [] -> error "No queries found to run setup migrations"
        ("" : xs) -> reverse xs
        commands -> reverse commands
  mapM_ (execute_ conn . Query) queryString
  colConf <- IOT.readFile "./app/defaultjson/conf.json"
  colModels <- IOT.readFile "./app/defaultjson/models.json"
  colDecks <- IOT.readFile "./app/defaultjson/decks.json"
  colDConf <- IOT.readFile "./app/defaultjson/dconf.json"

  execute
    conn
    (Query "INSERT INTO col VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?)")
    Col
      { idCol = 1,
        crtCol = 1411124400,
        modCol = 1425279151694,
        scmCol = 1425279151690,
        verCol = 11,
        dtyCol = 0,
        usnCol = 0,
        lsCol = 0,
        confCol = colConf,
        modelsCol = colModels,
        decksCol = colDecks,
        dconfCol = colDConf,
        tagsCol = "{}"
      }

  let colModelsObj = fromJust (decodeStrictText colModels :: Maybe Models)
      modelKeys = map (\x -> read (T.unpack (toText x)) :: Int) $ keys colModelsObj
  return modelKeys

dbPath :: FilePath
dbPath = "collection.anki2"

writeDbToApkg :: IO ()
writeDbToApkg = do
  let archivePath = "collection.anki2"
  entry <-
    toEntry
      archivePath
      . round
      <$> getPOSIXTime
      <*> BS.readFile dbPath
  let archive = addEntryToArchive entry emptyArchive
      archiveName = "test.apkg"
  BS.writeFile archiveName $ fromArchive archive

generateCollection :: [(T.Text, T.Text)] -> IO ()
generateCollection deck = do
  removeIfExists dbPath
  conn <- open dbPath
  modelKeys <- setupCollectionDb conn
  mapM_ (uncurry (addCard (head modelKeys) conn)) deck
  close conn
  writeDbToApkg
