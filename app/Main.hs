{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Collection.Generate
import Constructor (produceDeck)
import Data.Maybe (catMaybes)
import Data.Text (pack)
import Data.Text.IO qualified as T (getContents)
import System.Environment (getArgs)

data Opt
  = PlacehodlerOpt

data PankyArg
  = File String
  | Opt Opt

parseOpt :: String -> Maybe PankyArg
parseOpt _ = Nothing

main :: IO ()
main = do
  args <-
    catMaybes
      <$> ( map
              ( \case
                  ('-' : '-' : opt) -> parseOpt opt
                  ('-' : shortOpt) -> parseOpt shortOpt
                  file -> Just $ File file
              )
              <$> getArgs
          )

  let inputFiles = [file | File file <- args]
  let _ = [opt | Opt opt <- args]

  input <-
    if null args
      then T.getContents
      else return $ pack (head inputFiles)
  renderedCards <- produceDeck input

  generateCollection renderedCards
