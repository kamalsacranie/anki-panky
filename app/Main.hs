{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Collection.Generate
import Constructor (produceDeck)
import Control.Monad.State.Lazy (StateT (runStateT))
import Data.Maybe (catMaybes)
import Data.Text (pack)
import Data.Text.IO qualified as T (readFile)
import System.Directory (makeAbsolute)
import System.Environment (getArgs)
import System.FilePath (takeBaseName)
import Types.Anki

data Opt
  = PlacehodlerOpt
  deriving (Show)

data PankyArg
  = File FilePath
  | Opt Opt
  deriving (Show)

parseOpt :: String -> Maybe PankyArg
parseOpt _ = Nothing

handleFile :: FilePath -> IO ()
handleFile file = do
  let genInfo =
        DGInfo
          { filePath = file,
            deckFileName = takeBaseName (filePath genInfo),
            deckName = pack $ takeBaseName (filePath genInfo),
            deckId = Nothing,
            mediaDG = []
          }
  input <- T.readFile (filePath genInfo)
  (renderedCards, genInfoPostProd) <- runStateT (produceDeck input) genInfo

  generateCollection genInfoPostProd renderedCards

main :: IO ()
main = do
  maybeArgs <-
    mapM
      ( \case
          ('-' : '-' : opt) -> pure (parseOpt opt)
          ('-' : shortOpt) -> pure (parseOpt shortOpt)
          file -> Just <$> (File <$> makeAbsolute file)
      )
      <$> getArgs
  args <- catMaybes <$> maybeArgs

  let inputFiles = [file | File file <- args]
  let _ = [opt | Opt opt <- args]
  mapM_ handleFile inputFiles
