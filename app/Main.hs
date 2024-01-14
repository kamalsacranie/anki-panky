{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Control.Monad.State.Lazy
import Data.Text (Text)
import Data.Text.IO qualified as T
import Text.Pandoc

-- type Front = Text

data Card = Card [Inline] [Block]
  deriving (Show)

-- type Deck = [Card]

textToAst :: Text -> IO Pandoc
textToAst txt = do
  runIOorExplode $
    readMarkdown def {readerExtensions = pandocExtensions} txt

temp :: StateT [Block] IO [Block]
temp = do
  get >>= \case
    [] -> return []
    (x : xs) -> do
      case x of
        (Header 1 _ _) -> do
          put (x : xs)
          return []
        _ -> do
          put xs
          rest <- temp
          return $ x : rest

cardConstructor :: StateT [Block] IO Card
cardConstructor = do
  tem <- get
  let x = head tem
  put $ tail tem
  case x of
    (Header 1 _ c) -> do
      Card c <$> temp
    _ -> undefined

main :: IO ()
main = do
  (Pandoc _ blocks) <- T.getContents >>= textToAst
  (a, b) <- runStateT cardConstructor blocks
  print a
  return ()
