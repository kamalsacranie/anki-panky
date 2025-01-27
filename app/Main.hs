{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Collection.Generate
import Collection.Utils (handleMeta)
import Control.Monad.State
import Data.ByteString qualified as BS
import Data.Default (def)
import Data.Functor (($>))
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Text qualified as TS
import Data.Text.Encoding (decodeUtf8')
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.IO qualified as LTO (readFile)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Version (showVersion)
import Database.SQLite.Simple (Connection)
import GHC.IO.IOMode (IOMode (ReadMode))
import Paths_anki_panky (version)
import Render (normaliseAndExtractMedia, renderMDtoNative, renderPandocAsDecks)
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
    makeAbsolute,
  )
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (takeBaseName, takeDirectory, (</>))
import System.IO (withBinaryFile)
import System.Posix.Temp
import Types
  ( DeckGenInfo (..),
    MediaDeck,
    MediaItem,
    PankyApp,
    SpecialFileInfo (..),
  )
import Types.CLI
import Utils (splitListOnce)

-- | Checks if the input file is a valid deck file
-- | TODO: Change this implementation to handle an IO exception with readFile from Lazy Text
isValidFile :: BS.ByteString -> Bool
isValidFile input = case decodeUtf8' input of
  Left _ -> False
  Right res -> case TS.unpack res of
    ('-' : '-' : '-' : '\n' : _) -> True
    ('#' : ' ' : _) -> True
    _anyOtherFirstLine -> False

handleDeck :: Connection -> [Int] -> DeckFile -> IO [MediaItem]
handleDeck conn modelKeys dfs@(InputFile path _) = do
  byteTestInput <- withBinaryFile path ReadMode $ \h ->
    ( do
        BS.take 1000 <$> BS.hGetContents h
    )
  if isValidFile byteTestInput
    then
      ( do
          handleDeck' conn modelKeys dfs
      )
    else return []

handleDeck' :: Connection -> [Int] -> DeckFile -> IO [MediaItem]
handleDeck' conn modelKeys (InputFile path deckPrefix) = do
  miliEpoc :: Int <- floor . (* 10000) <$> getPOSIXTime
  let genInfoDefault =
        DGInfo
          { deckPath = path,
            deckFileName = takeBaseName path,
            deckName = T.pack (show deckPrefix ++ takeBaseName path),
            deckId = miliEpoc
          }
  input <- LTO.readFile path
  doc <- renderMDtoNative input
  (normalisedDoc, mediaFiles) <- normaliseAndExtractMedia doc path
  renderedDeck <- renderPandocAsDecks normalisedDoc
  (_, genInfo) <- runStateT (handleMeta doc deckPrefix) genInfoDefault
  if null renderedDeck
    then print ("Skipping file " ++ path ++ " as it failed to parse its cards") $> []
    else generateDeck conn modelKeys renderedDeck genInfo $> mediaFiles

dbPath :: IO FilePath
dbPath = do
  temppath <- mkdtemp "/tmp/anki-panky"
  let filepath = temppath </> "collection.anki2"
  return filepath

handleCol :: [DeckFile] -> T.Text -> PankyApp ()
handleCol deckFiles colName = do
  dbpath <- liftIO dbPath
  c <- liftIO $ createCollectionDb dbpath
  modelKeys <- setupCollectionDb c
  mediaFiles <- liftIO $ foldM (\mfiles deck -> (mfiles ++) <$> handleDeck c modelKeys deck) [] deckFiles
  let mediaDeck :: MediaDeck = zip [0 :: Int ..] mediaFiles
  writeDbToApkg mediaDeck colName dbpath

takeBasePathName :: FilePath -> String
takeBasePathName path = case reverse path of
  ('/' : rest) -> takeBaseName $ takeDirectory (reverse rest)
  _nonDirStylePath -> takeBaseName path

parseSpecialFile :: FilePath -> State SpecialFileInfo ()
parseSpecialFile ".pankyignore" = pure ()
parseSpecialFile ('.' : deckName) = modify (\s -> s {sfDeckNameFile = Just deckName})
parseSpecialFile _ = pure ()

parseSpecialFiles :: [FilePath] -> State SpecialFileInfo ()
parseSpecialFiles =
  foldr ((*>) . parseSpecialFile) (pure ())

constructDeckTree' :: FilePath -> [T.Text] -> IO [DeckFile]
constructDeckTree' path prefList = do
  paths <- listDirectory path
  specialFiles <-
    filterM
      ( \fp -> doesDirectoryExist (path </> fp) >>= (return . not)
      )
      [p | p <- paths, case p of ('.' : _) -> True; _nonSpecial -> False]

  filesToIgnore <- if ".pankyignore" `elem` specialFiles then LTO.readFile (path </> ".pankyignore") >>= (return . T.lines) else pure []

  let (_, specialFileInfo) =
        runState (parseSpecialFiles [p | p <- specialFiles, T.pack p `notElem` filesToIgnore]) def
  let deckName = fromMaybe (takeBasePathName path) $ sfDeckNameFile specialFileInfo
      prefix' = prefList ++ [T.pack deckName]

  let filesToProcess = [path </> p | p <- paths, (case p of ('.' : _) -> False; _nonSpecialFile -> True) && notElem (T.pack p) filesToIgnore]
  deckFiless <- mapM (`constructDeckTree` prefix') filesToProcess
  return $ concat deckFiless

constructDeckTree :: FilePath -> [T.Text] -> IO [DeckFile]
constructDeckTree path prefList =
  doesDirectoryExist path >>= \case
    True -> constructDeckTree' path prefList
    False -> return [InputFile path (DPos prefList)]

type ArgumentDescription = String

showHelp :: [Char]
showHelp =
  let notHelp = (`notElem` ["-help", "h"]) . fst
      entryToDescription = (\(arg, desc) -> "-" ++ arg ++ ": " ++ desc) . fmap snd
   in (intercalate "\n" . map entryToDescription) (filter notHelp parsePankyOption)

parsePankyOption :: [(String, (PankyOption, ArgumentDescription))]
parsePankyOption =
  [ ("-version", (Flag Version, versionDescription)),
    ("v", (Flag Version, versionDescription)),
    ("-verbose", (Flag Verbose, verboseDescription)),
    ("V", (Flag Verbose, verboseDescription)),
    ("-name", (Opt DeckName, nameDescription)),
    ("-output", (Opt OutputDir, outputDescription)),
    ("o", (Opt OutputDir, outputDescription)),
    ("-css", (Opt CSSExtend, extendCssDescription)),
    ("+css", (Opt CSSOverride, overrideCssDescription)),
    ("-help", (Flag Help, showHelp)),
    ("h", (Flag Help, showHelp))
  ]
  where
    versionDescription = "Print version"
    verboseDescription = "Print verbose output"
    nameDescription = "Set the name of the deck"
    outputDescription = "Set the output directory"
    extendCssDescription = "Extend the default CSS"
    overrideCssDescription = "Override the default CSS"

parseArgs :: [String] -> [PankyArg]
parseArgs [] = []
parseArgs ['-' : optString] = case fst <$> lookup optString parsePankyOption of
  Just (Flag flag) -> [PFlag flag]
  Just (Opt kwarg) -> error $ "Option without value " ++ show kwarg
  Nothing -> error $ "Invalid CLI arg -" ++ optString
parseArgs [x] = [SourcePath x]
parseArgs (('-' : optString) : optv : xs) = case fst <$> lookup optString parsePankyOption of
  Just (Flag flag) -> PFlag flag : parseArgs (optv : xs)
  Just (Opt kwarg) -> POpt kwarg (T.pack optv) : parseArgs xs
  Nothing -> error $ "Invalid CLI arg -" ++ optString
parseArgs (file : xs) = SourcePath file : parseArgs xs

interpretAsTextOrReadFile :: T.Text -> IO T.Text
interpretAsTextOrReadFile rawTextOrFilePath =
  doesFileExist fileName
    >>= ( \case
            True -> LTO.readFile fileName
            False -> return rawTextOrFilePath
        )
  where
    fileName = T.unpack rawTextOrFilePath

constructPankyConfFromArgs :: [PankyArg] -> IO PankyConfig
constructPankyConfFromArgs opts = do
  outputDir <- makeAbsolute $ T.unpack $ case [dir | POpt OutputDir dir <- opts] of
    [] -> "."
    (dir : _) -> dir
  -- refactor to be more idiomatic
  cssExtendRaw <- case [argVal | POpt CSSExtend argVal <- opts] of
    [] -> pure ""
    (cssExtendArgVal : _) -> interpretAsTextOrReadFile cssExtendArgVal
  cssOverrideRaw <- case [argVal | POpt CSSOverride argVal <- opts] of
    [] -> pure ""
    (cssOverrideArgVal : _) -> interpretAsTextOrReadFile cssOverrideArgVal
  when
    (cssExtendRaw /= "" && cssOverrideRaw /= "")
    $ error
      "Cannot extend the default CSS and override the default CSS simultaneously"
  return $
    PankyConfig
      { outputDirPConf = outputDir,
        cssExtendPConf = cssExtendRaw,
        cssOverridePConf = cssOverrideRaw
      }

useage :: String
useage = "anki-pany [flags/options] input"

main :: IO ()
main = do
  rawArgs <- concatMap (splitListOnce '=') <$> getArgs
  let args = parseArgs rawArgs

  inputSources <- mapM makeAbsolute [source | SourcePath source <- args]
  let opts = [arg | arg <- args, (case arg of SourcePath _ -> False; _nonFileArg -> True)]

  when (opts == [PFlag Help]) $
    putStrLn "Available options for anki-panky are:\n"
      *> putStrLn useage
      *> putStrLn showHelp
      <* exitSuccess
  when (PFlag Help `elem` opts) $
    putStrLn "You cannot pass the help flag with any other flags:\n"
      *> putStrLn useage
      *> putStrLn showHelp
      <* exitFailure
  when (PFlag Version `elem` opts) $ putStrLn (showVersion version) <* exitSuccess

  pankyConf <- constructPankyConfFromArgs opts

  createDirectoryIfMissing True $ outputDirPConf pankyConf

  trees <-
    mapM
      ( \sourcePath -> ColDir sourcePath <$> constructDeckTree sourcePath []
      )
      inputSources

  when (null trees) $
    putStrLn "You must provide one or more input files. Command useage:\n"
      *> putStrLn useage
      *> putStrLn showHelp
      <* exitFailure
  mapM_
    ( \case
        ColDir fp [] -> putStrLn $ "Skipping invalid input file: " ++ fp ++ " as it is empty"
        ColDir path cds ->
          let colName = T.pack (takeBasePathName path)
           in runStateT (handleCol cds colName) pankyConf $> ()
    )
    trees
