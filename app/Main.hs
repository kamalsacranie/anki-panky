{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import CardParser.ExtCard (extendedCard)
import CardParser.Parser
import CardParser.SimpleCard
import Codec.Archive.Zip
import Control.Applicative
import Control.Monad (when)
import Data.ByteString.Lazy qualified as BS
import Data.Char (chr)
import Data.Text qualified as T
import Data.Text.IO qualified as IOT
import Data.Time.Clock.POSIX
import Database.SQLite.Simple
import System.Directory (doesFileExist, removeFile)
import System.Directory.Internal.Prelude (getArgs)
import System.Random
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Text.Pandoc hiding (getPOSIXTime)
import Types.Anki as A
import Types.Parser as P

textToAst :: T.Text -> IO Pandoc
textToAst txt = do
  runIOorExplode $
    readMarkdown def {readerExtensions = pandocExtensions} txt

cards :: Parser [P.Card]
cards = many (extendedCard <|> simpleCard)

documenttizeDeck :: ([Block] -> Pandoc) -> [P.Card] -> [(Pandoc, Pandoc)]
documenttizeDeck document =
  map
    ( \case
        (P.Card (SimpleFront f) fv) -> (document [f], document fv)
        (P.Card (ExtendedFront f) fv) -> (document f, document fv)
    )

writeFlashCardHtml :: (PandocMonad m) => Pandoc -> m Html
writeFlashCardHtml =
  writeHtml5
    ( def
        { writerExtensions = pandocExtensions,
          writerHTMLMathMethod = MathJax defaultMathJaxURL
        }
    )

renderDeck :: [(Pandoc, Pandoc)] -> [(String, String)]
renderDeck =
  map
    ( \(f, b) -> case (writeFlashCardHtml f, writeFlashCardHtml b) of
        (Right front, Right back) -> (renderHtml front, renderHtml back)
        _ -> error "Fucked it"
    )

-- | Returns a list of tuples which contain rendered HTML for the front and
-- | back of the cards provided in the order that they appeared
pandocPart :: T.Text -> IO [(String, String)]
pandocPart input = do
  (Pandoc meta blocks) <- textToAst input
  let deck = concat $ parseAll cards blocks
  let docDeck = documenttizeDeck (Pandoc meta) deck
  let renderedDeck = renderDeck docDeck
  return renderedDeck

removeIfExists :: FilePath -> IO ()
removeIfExists filePath = do
  exists <- doesFileExist filePath
  when exists $ removeFile filePath

dbStuff :: [(String, String)] -> IO ()
dbStuff deck = do
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

  -------- Default sestup

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

  ---------
  let (front, back) = undefined

  gen <- getStdGen
  let randomInt :: Int
      randomInt = fst $ random gen

  execute
    conn
    "INSERT INTO notes VALUES(?,?,?,?,?,?,?,?,?,?,?)"
    A.Note
      { idNote = 1706664369114,
        guidNote = T.pack $ show randomInt,
        midNote = 1691663570,
        modNote = 1706664369,
        usnNote = -1,
        tagsNote = "",
        fldsNote = T.pack (front ++ [chr 0x1f] ++ back ++ [chr 0x1f] ++ show randomInt),
        sfldNote = T.pack "<h2>This is the front</h2>",
        csumNote = 0,
        flagsNote = 0,
        dataNote = ""
      }

  t <- round . (* 1000) <$> getPOSIXTime :: IO Int
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

  entry <-
    toEntry
      "collection.anki2"
      . round
      <$> getPOSIXTime
      <*> BS.readFile "./collection.anki2"
  let archive = addEntryToArchive entry emptyArchive
  let archiveName = "test.apkg"
  BS.writeFile archiveName $ fromArchive archive

main :: IO ()
main = do
  a <- getArgs
  -- if our args list is empty
  input <-
    if null a
      then IOT.getContents
      else return $ T.pack (head a)
  renderedCards <- pandocPart input

  dbStuff renderedCards
  return ()

-- [[decksJSON :: TT.Text]] <- query_ conn (Query "SELECT decks FROM col")
-- let deckList' = fromJust $ decodeStrictText decksJSON :: DeckList
-- t <- T.readFile "./app/deck.json"
-- let deck = fromJust $ decodeStrictText t :: Deck
-- let deckList = KeyMap.insert "1" deck deckList'
-- execute conn (Query "UPDATE col SET decks = ?") (Only $ encode deckList)

-- [[modelsJSON :: TT.Text]] <- query_ conn (Query "SELECT models FROM col")
-- execute conn (Query "UPDATE col SET models = ?") (Only modelsJSON)
