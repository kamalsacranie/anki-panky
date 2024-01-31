{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import CardParser.ExtCard (extendedCard)
import CardParser.Parser
import CardParser.SimpleCard
import Control.Applicative
import Control.Monad (when)
import Data.Char (chr)
import Data.Text qualified as T
import Data.Text.IO qualified as IOT
import Database.SQLite.Simple
import System.Directory (doesFileExist, removeFile)
import Text.Pandoc
import Types.Anki as A
import Types.Parser as P

textToAst :: T.Text -> IO Pandoc
textToAst txt = do
  runIOorExplode $
    readMarkdown def {readerExtensions = pandocExtensions} txt

cards :: Parser [P.Card]
cards = many (extendedCard <|> simpleCard)

pandocPart = do
  (Pandoc meta blocks) <- IOT.getContents >>= textToAst
  let deck = concat $ parseAll cards blocks
      (front, back) = case head deck of
        (P.Card (SimpleFront f) fv) -> (Pandoc meta [f], Pandoc meta fv)
        (P.Card (ExtendedFront f) fv) -> (Pandoc meta f, Pandoc meta fv)
  _ <- runIO $ writeHtml5 def {writerExtensions = pandocExtensions, writerHTMLMathMethod = MathJax defaultMathJaxURL} front
  _ <- runIO $ writeHtml5 def {writerExtensions = pandocExtensions, writerHTMLMathMethod = MathJax defaultMathJaxURL} back
  return ()

removeIfExists :: FilePath -> IO ()
removeIfExists filePath = do
  exists <- doesFileExist filePath
  when exists $ removeFile filePath

main :: IO ()
main = do
  -- a <- getArgs
  -- -- if our args list is empty
  -- input <-
  --   if null a
  --     then IOT.getContents
  --     else return $ T.pack (head a)
  -- pandocPart

  let dbPath = "collection.anki2"
  removeIfExists dbPath
  conn <- open dbPath
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

  execute
    conn
    "INSERT INTO notes VALUES(?,?,?,?,?,?,?,?,?,?,?)"
    A.Note
      { idNote = 1706664369114,
        guidNote = "MlOX*&baDU",
        midNote = 1691663570,
        modNote = 1706664369,
        usnNote = -1,
        tagsNote = "",
        fldsNote = T.pack ("This is the front" ++ [chr 0x1f] ++ "This is the back" ++ [chr 0x1f] ++ "MlOX*&baDU"),
        sfldNote = T.pack "<h2>This is the front</h2>",
        csumNote = 0,
        flagsNote = 0,
        dataNote = ""
      }

  execute
    conn
    "INSERT INTO cards VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
    A.Card
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
