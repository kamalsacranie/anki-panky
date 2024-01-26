{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Control.Applicative
import Control.Monad.State.Lazy
import Data.Functor
import Data.Text (Text)
import Data.Text.IO qualified as T
import Text.Pandoc

data Front
  = SimpleFront [Inline]
  | ExtendedFront [Inline] [Block]
  deriving (Show)

type Back = [Block]

data Card
  = SimpleCard Front Back
  | ExtendedCard Front Back
  deriving (Show)

textToAst :: Text -> IO Pandoc
textToAst txt = do
  runIOorExplode $
    readMarkdown def {readerExtensions = pandocExtensions} txt

constructCardBody :: StateT [Block] Maybe [Block]
constructCardBody =
  ( do
      peek >>= \case
        Header 1 _ _ -> do
          return []
        Para [Str ".", Space, Str ".", Space, Str "."] -> do
          return []
        _ -> do
          x <- pop
          rest <- constructCardBody
          return $ x : rest
  )
    <|> return []

peek :: StateT [a] Maybe a
peek =
  get >>= \case
    [] -> lift Nothing
    (x : _) -> return x

pop :: StateT [a] Maybe a
pop =
  get >>= \case
    [] -> lift Nothing
    (x : xs) -> (put xs $> x)

isEmpty :: StateT [a] Maybe Bool
isEmpty =
  get >>= \case
    [] -> return True
    _ -> return False

constructCards :: StateT [Block] Maybe [Card]
constructCards =
  pop >>= \case
    Header 1 _ c ->
      constructCardBody >>= \body ->
        ( peek
            >>= ( \case
                    Para [Str ".", Space, Str ".", Space, Str "."] -> pop *> (ExtendedCard (ExtendedFront c body) <$> constructCardBody)
                    Header 1 _ _ -> return (SimpleCard (SimpleFront c) body)
                    _ -> error "Incorrect syntax error msg tbd"
                )
            >>= \card ->
              tryConstructCards
                >>= \cards -> return $ card : cards
        )
          <|> return [SimpleCard (SimpleFront c) body]
    x -> error $ "Incorrect syntax on line with text: " ++ show x

try :: StateT [Block] Maybe a -> a -> StateT [Block] Maybe a
try m x = m <|> return x

tryConstructCards :: StateT [Block] Maybe [Card]
tryConstructCards = try constructCards []

main :: IO ()
main = do
  (Pandoc _ blocks) <- T.getContents >>= textToAst
  let (cards, blocks') = case runStateT tryConstructCards blocks of
        Just x -> x
        Nothing -> error "Incorrect syntax"
  print ("Remaining blocks: " :: String) *> print blocks'
  print cards
  return ()
