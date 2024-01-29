{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import CardParser.ExtCard
import CardParser.Parser
import CardParser.SimpleCard
import Control.Applicative
import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as B
import Data.Maybe (fromJust)
import Data.Text qualified as TT
import Data.Text.IO qualified as T
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Database.SQLite.Simple
import System.Exit (exitSuccess)
import Temp
import Text.Pandoc
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
  conn <- open ""
  -- read text from file
  queries <- T.readFile "./app/temp.sql"
  let queryString = init $ TT.splitOn ";" queries
  mapM_ (execute_ conn . Query) queryString
  [[decksJSON :: TT.Text]] <- query_ conn (Query "SELECT decks FROM col")
  let deckList' = fromJust $ decodeStrictText decksJSON :: DeckList
  t <- T.readFile "./app/deck.json"
  let temp = fromJust $ decodeStrictText t :: Deck
  let deckList = KeyMap.insert "1" temp deckList'
  execute conn (Query "UPDATE col SET decks = ?") (Only $ encode deckList)

  [[modelsJSON :: TT.Text]] <- query_ conn (Query "SELECT models FROM col")
  T.readFile "./app/model.json" >>= print
  execute conn (Query "UPDATE col SET models = ?") (Only modelsJSON)
  (query_ conn (Query "SELECT * FROM col") :: IO [Col]) >>= print

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
    decks :: ByteString,
    dconf :: TT.Text,
    tags :: TT.Text
  }
  deriving (Show, Generic, FromRow)

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
