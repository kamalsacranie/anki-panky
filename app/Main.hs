{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import CardParser.ExtCard
import CardParser.Parser
import CardParser.SimpleCard
import Codec.Archive.Zip
import Control.Applicative
import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as B
import Data.Char (chr, ord)
import Data.Maybe (fromJust)
import Data.Text qualified as TT
import Data.Text.IO qualified as T
import Data.Text.Lazy (fromStrict, pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Database.SQLite.Simple
import GHC.Generics
import System.Exit (exitSuccess)
import Temp
import Text.Pandoc hiding (Note)
import Types

textToAst :: TT.Text -> IO Pandoc
textToAst txt = do
  runIOorExplode $
    readMarkdown def {readerExtensions = pandocExtensions} txt

cards :: Parser [Card]
cards = many (extendedCard <|> simpleCard)

main :: IO ()
main = do
  (Pandoc meta blocks) <- T.getContents >>= textToAst
  let a = concat $ parseAll cards blocks
      (front, back) = case head a of
        (Card (SimpleFront f) fv) -> (Pandoc meta [f], Pandoc meta fv)
        (Card (ExtendedFront f) fv) -> (Pandoc meta f, Pandoc meta fv)
  _ <- runIO $ writeHtml5 def {writerExtensions = pandocExtensions, writerHTMLMathMethod = MathJax defaultMathJaxURL} front
  _ <- runIO $ writeHtml5 def {writerExtensions = pandocExtensions, writerHTMLMathMethod = MathJax defaultMathJaxURL} back
  conn <- open "temp.db"
  -- read text from file
  queries <- T.readFile "./app/temp.sql"
  let queryString = init $ TT.splitOn ";" queries
  mapM_ (execute_ conn . Query) queryString

  colConf <- T.readFile "./app/defaultjson/conf.json"
  colModels <- T.readFile "./app/defaultjson/models.json"
  colDecks <- T.readFile "./app/defaultjson/decks.json"
  colDConf <- T.readFile "./app/defaultjson/dconf.json"

  execute
    conn
    (Query "INSERT INTO col VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?)")
    (Col 1 1411124400 1425279151694 1425279151690 11 0 0 0 colConf colModels colDecks colDConf "{}")

  -- [[decksJSON :: TT.Text]] <- query_ conn (Query "SELECT decks FROM col")
  -- let deckList' = fromJust $ decodeStrictText decksJSON :: DeckList
  -- t <- T.readFile "./app/deck.json"
  -- let deck = fromJust $ decodeStrictText t :: Deck
  -- let deckList = KeyMap.insert "1" deck deckList'
  -- execute conn (Query "UPDATE col SET decks = ?") (Only $ encode deckList)

  -- [[modelsJSON :: TT.Text]] <- query_ conn (Query "SELECT models FROM col")
  -- execute conn (Query "UPDATE col SET models = ?") (Only modelsJSON)

  -- temp <- B.readFile "./temp.db"
  -- let entry = toEntry "collection.anki2" 0 temp
  -- let arch' = emptyArchive
  -- let arch = addEntryToArchive entry arch'
  -- B.writeFile "test.apkg" (fromArchive arch)

  execute
    conn
    "INSERT INTO notes VALUES(?,?,?,?,?,?,?,?,?,?,?)"
    Note
      { idNote = 1706664369114,
        guidNote = "MlOX*&baDU",
        midNote = 1691663570,
        modNote = 1706664369,
        usnNote = -1,
        tagsNote = "",
        fldsNote = TT.pack ("This is the front" ++ [chr 0x1f] ++ "This is the back" ++ [chr 0x1f] ++ "MlOX*&baDU"),
        sfldNote = TT.pack "<h2>This is the front</h2>",
        csumNote = 0,
        flagsNote = 0,
        dataNote = ""
      } ::
    IO ()

  execute
    conn
    "INSERT INTO cards VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
    DBCard
      { idCard = 1706664369115,
        nidCard = 1706664369114,
        didCard = 1288033183,
        ordCard = 0,
        modCard = 1706664369,
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

  close conn
  return ()

data Col = Col
  { id :: Int,
    crt :: Int,
    mod :: Int,
    scm :: Int,
    ver :: Int,
    dty :: Int,
    usn :: Int,
    ls :: Int,
    conf :: TT.Text,
    models :: TT.Text,
    decks :: TT.Text,
    dconf :: TT.Text,
    tags :: TT.Text
  }
  deriving (Show, Generic, FromRow, ToRow)

-- data Col = Col Int Int Int Int Int Int Int Int TT.Text TT.Text ByteString TT.Text TT.Text deriving (Show, Generic, FromRow)

-- instance FromRow Col where
--   fromRow =
--     Col
--       <$> field
--       <*> field
--       <*> field
--       <*> field
--       <*> field
--       <*> field
--       <*> field
--       <*> field
--       <*> field
--       <*> field
--       <*> field
--       <*> field
--       <*> field

data Note = Note
  { idNote :: Int,
    guidNote :: TT.Text,
    midNote :: Int,
    modNote :: Int,
    usnNote :: Int,
    tagsNote :: TT.Text,
    fldsNote :: TT.Text,
    sfldNote :: TT.Text,
    csumNote :: Int,
    flagsNote :: Int,
    dataNote :: TT.Text
  }
  deriving (Show, Generic, FromRow, ToRow)

-- id              integer primary key,
-- crt             integer not null,
-- mod             integer not null,
-- scm             integer not null,
-- ver             integer not null,
-- dty             integer not null,
-- usn             integer not null,
-- ls              integer not null,
-- conf            text not null,
-- models          text not null,
-- decks           text not null,
-- dconf           text not null,
-- tags            text not null

-- 1,
-- 1411124400,
-- 1425279151694,
-- 1425279151690,
-- 11,
-- 0,
-- 0,
-- 0,
