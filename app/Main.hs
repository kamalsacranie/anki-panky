{-# LANGUAGE ScopedTypeVariables #-}

import Collection.Generate
import Constructor (produceDeck)
import Control.Monad.State.Lazy (StateT (runStateT), when)
import Data.Text (pack)
import Data.Text.Lazy qualified as LT (takeWhile, toStrict, unpack)
import Data.Text.Lazy.IO qualified as LTO (readFile)
import System.Directory (makeAbsolute)
import System.Environment (getArgs)
import System.FilePath (takeBaseName)
import Types.Anki

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
  input <- LTO.readFile (filePath genInfo)
  let firstLine = LT.takeWhile (/= '\n') input
  let isValidFile = case LT.unpack firstLine of
        "---" -> True
        ('#' : ' ' : _) -> True
        _ -> False
  when isValidFile $ do
    (renderedCards, genInfoPostProd) <- runStateT (produceDeck (LT.toStrict input)) genInfo
    if null renderedCards
      then putStrLn $ "Skipping file \"" ++ deckFileName genInfoPostProd ++ "\" as it failed to parse"
      else generateCollection genInfoPostProd renderedCards

splitStringOnce :: Char -> String -> [String]
splitStringOnce _ "" = []
splitStringOnce delimiter str =
  let (first, remainder) = break (== delimiter) str
   in first : case remainder of
        (_ : rest) -> [rest]
        [] -> []

main :: IO ()
main = do
  rawArgs <- concatMap (splitStringOnce '=') <$> getArgs
  let args = parseArgs rawArgs

  let inputFiles = [file | File file <- args]
  absInputFiles <- mapM makeAbsolute inputFiles
  let _opts = [arg | arg <- args, (case arg of File _ -> False; _ -> True)]
  mapM_ handleFile absInputFiles

data PankyFlag
  = Verbose
  deriving (Show)

data PankyKWarg
  = DeckName
  deriving (Show)

data PankyOption
  = Flag PankyFlag
  | Opt PankyKWarg
  deriving (Show)

data PankyArg
  = File FilePath
  | PFlag PankyFlag
  | POpt PankyKWarg String
  deriving (Show)

parsePankyOption :: String -> Maybe PankyOption
parsePankyOption "V" = return $ Flag Verbose
parsePankyOption "-verbose" = return $ Flag Verbose
parsePankyOption "-name" = return $ Opt DeckName
parsePankyOption _ = Nothing

parseArgs :: [String] -> [PankyArg]
parseArgs [] = []
parseArgs ['-' : optString] = case parsePankyOption optString of
  Just (Flag flag) -> [PFlag flag]
  Just (Opt _) -> error "Option without value"
  Nothing -> error $ "Invalid CLI arg -" ++ optString
parseArgs [x] = [File x]
parseArgs (('-' : optString) : optv : xs) = case parsePankyOption optString of
  Just (Flag flag) -> PFlag flag : parseArgs (optv : xs)
  Just (Opt kwarg) -> POpt kwarg optv : parseArgs xs
  Nothing -> error $ "Invalid CLI arg -" ++ optString
parseArgs (file : xs) = File file : parseArgs xs
