{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Collection.Generate
import Constructor (produceDeck)
import Data.Maybe (catMaybes)
import Data.Text.IO qualified as T (readFile)
import System.Environment (getArgs)
import System.FilePath (takeBaseName)
import Types.Anki

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

  -- in single file operation, the name of the file is the name of the deck

  -- probably going to want to use a state monad right
  let genInfo =
        DGInfo
          { filePath = head inputFiles,
            deckFileName = takeBaseName (filePath genInfo),
            deckName = takeBaseName (filePath genInfo)
          }
  input <- T.readFile (filePath genInfo)
  renderedCards <- produceDeck input

  generateCollection renderedCards genInfo
