{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Collection.Generate
import Collection.Utils (handleMeta)
import Control.Monad.State.Lazy
import Data.ByteString qualified as BS
import Data.Functor (($>))
import Data.Text.Lazy qualified as T
import Data.Text qualified as TS
import Data.Text.Encoding (decodeUtf8')
import Data.Text.Lazy.IO qualified as LTO (readFile)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Version (showVersion)
import Database.SQLite.Simple (Connection)
import Paths_anki_panky (version)
import Render (normaliseAndExtractMedia, renderMDtoNative, renderPandocAsDecks)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory, makeAbsolute, doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.FilePath (takeBaseName, takeDirectory, (</>))
import System.Posix.Temp
import Types (DeckGenInfo (..), MediaDeck, MediaItem, PankyApp)
import Types.CLI
import Utils (splitListOnce)
import GHC.IO.IOMode (IOMode(ReadMode))
import System.IO (withBinaryFile)

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
  byteTestInput <- withBinaryFile path ReadMode $ \h -> (do
      BS.take 1000 <$> BS.hGetContents h)
  if isValidFile byteTestInput
    then (do
      handleDeck' conn modelKeys dfs)
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

constructDeckTree' :: FilePath -> [T.Text] -> IO [DeckFile]
constructDeckTree' path prefList = do
  paths <- listDirectory path
  let specialFiles = [p | p <- paths, case p of ('.' : _) -> True; _nonSpecial -> False]
      filesToProcess = [path </> p | p <- paths, case p of ('.' : _) -> False; _nonSpecialFile -> True]
  let deckName =
        if not (null specialFiles)
          then tail (head specialFiles)
          else takeBasePathName path
  let prefix' = prefList ++ [T.pack deckName]
  deckFiless <- mapM (`constructDeckTree` prefix') filesToProcess
  return $ concat deckFiless

constructDeckTree :: FilePath -> [T.Text] -> IO [DeckFile]
constructDeckTree path prefList =
  doesDirectoryExist path >>= \case
    True -> constructDeckTree' path prefList
    False -> return [InputFile path (DPos prefList)]

parsePankyOption :: String -> Maybe PankyOption
parsePankyOption "V" = return $ Flag Verbose
parsePankyOption "-verbose" = return $ Flag Verbose
parsePankyOption "v" = return $ Flag Version
parsePankyOption "-version" = return $ Flag Version
parsePankyOption "-name" = return $ Opt DeckName
parsePankyOption "-output" = return $ Opt OutputDir
parsePankyOption "o" = return $ Opt OutputDir
parsePankyOption "+css" = return $ Opt CSSExtend
parsePankyOption "-css" = return $ Opt CSSOverride
parsePankyOption _ = Nothing

parseArgs :: [String] -> [PankyArg]
parseArgs [] = []
parseArgs ['-' : optString] = case parsePankyOption optString of
  Just (Flag flag) -> [PFlag flag]
  Just (Opt kwarg) -> error $ "Option without value " ++ show kwarg
  Nothing -> error $ "Invalid CLI arg -" ++ optString
parseArgs [x] = [SourcePath x]
parseArgs (('-' : optString) : optv : xs) = case parsePankyOption optString of
  Just (Flag flag) -> PFlag flag : parseArgs (optv : xs)
  Just (Opt kwarg) -> POpt kwarg (T.pack optv) : parseArgs xs
  Nothing -> error $ "Invalid CLI arg -" ++ optString
parseArgs (file : xs) = SourcePath file : parseArgs xs

interpretAsTextOrReadFile :: T.Text -> IO T.Text
interpretAsTextOrReadFile rawTextOrFilePath = doesFileExist fileName >>= (\case
                                True -> LTO.readFile fileName
                                False -> return rawTextOrFilePath)
  where fileName = T.unpack rawTextOrFilePath

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
  return $ PankyConfig {
      outputDirPConf = outputDir
    , cssExtendPConf = cssExtendRaw
    , cssOverridePConf = cssOverrideRaw
    }

main :: IO ()
main = do
  rawArgs <- concatMap (splitListOnce '=') <$> getArgs
  let args = parseArgs rawArgs

  inputSources <- mapM makeAbsolute [source | SourcePath source <- args]
  let opts = [arg | arg <- args, (case arg of SourcePath _ -> False; _nonFileArg -> True)]

  when (PFlag Version `elem` opts) $ putStrLn (showVersion version) *> exitSuccess

  pankyConf <- constructPankyConfFromArgs opts

  createDirectoryIfMissing True $ outputDirPConf pankyConf

  trees <-
    mapM
      ( \sourcePath -> ColDir sourcePath <$> constructDeckTree sourcePath []
      )
      inputSources

  when (null trees) $ error "No input files found"
  mapM_
    ( \case
        ColDir fp [] -> print $ "Skipping invalid input file: " ++ fp ++ " as it is empty"
        ColDir path cds ->
          let colName = T.pack (takeBasePathName path)
           in runStateT (handleCol cds colName) pankyConf $> ()
    )
    trees
