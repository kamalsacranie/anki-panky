import CardParser.ExtCard
import CardParser.Parser
import CardParser.SimpleCard
import Control.Applicative
import Data.Text (Text)
import Data.Text.IO qualified as T
import Text.Pandoc
import Types

textToAst :: Text -> IO Pandoc
textToAst txt = do
  runIOorExplode $
    readMarkdown def {readerExtensions = pandocExtensions} txt

cards :: Parser [Card]
cards = many (extendedCard <|> simpleCard)

main :: IO ()
main = do
  (Pandoc _ blocks) <- T.getContents >>= textToAst
  let a = parseAll cards blocks
  print a
  return ()
