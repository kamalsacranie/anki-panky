{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Collection.Generate
import Constructor (produceDeck, textToAst)
import Control.Monad.State.Lazy
import Data.Aeson (decodeFileStrict, decodeStrictText)
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as AKM
import Data.Aeson.Text (encodeToLazyText)
import Data.ByteString.Lazy qualified as BL
import Data.List (intercalate)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT (toStrict, unpack)
import Data.Text.Lazy.Encoding (decodeUtf8')
import Data.Text.Lazy.IO qualified as LTO (readFile)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.SQLite.Simple (Connection, Only (Only), Query (Query), execute, query_)
import System.Directory (doesDirectoryExist, listDirectory, makeAbsolute)
import System.Environment (getArgs)
import System.FilePath (takeBaseName, takeDirectory)
import Types (DeckGenInfo (..), Panky)
import Types.Anki.JSON (Deck (..), Decks)

-- | Checks if the input file is a valid deck file
-- | TODO: Change this implementation to handle an IO exception with readFile from Lazy Text
isValidFile :: BL.ByteString -> Bool
isValidFile input = case decodeUtf8' input of
  Left _ -> False
  Right res -> case LT.unpack res of
    ('-' : '-' : '-' : '\n' : _) -> True
    ('#' : ' ' : _) -> True
    _anyOtherFirstLine -> False

handleDeck :: Connection -> [Int] -> DeckFile -> Panky ()
handleDeck conn modelKeys (InputFile path deckPrefix) = do
  byteTestInput <- liftIO $ BL.take 1000 <$> BL.readFile path
  when (isValidFile byteTestInput) $ do
    input <- liftIO $ LTO.readFile path
    doc <- liftIO $ textToAst $ LT.toStrict input
    modify (\s -> s {filePath = Just path})
    renderedCards <- produceDeck doc
    if null renderedCards
      then liftIO $ putStrLn $ "Skipping file \"" ++ path ++ "\" as it failed to parse its cards"
      else do
        currTime <- liftIO getPOSIXTime
        let miliEpoc = floor $ currTime * 10000 :: Int
        modify
          ( \s ->
              s
                { deckFileName = Just $ takeBaseName path,
                  deckName = Just $ T.pack (show deckPrefix ++ "::" ++ takeBaseName path),
                  deckId = Just miliEpoc
                }
          )
        colDeckDefault <- liftIO $ fromJust <$> (decodeFileStrict "./data/default-anki-json/deck.json" :: IO (Maybe Deck))
        dId <- gets (fromMaybe (error "No Deck Id set during generation.") . deckId)
        dName <- gets (fromMaybe (error "No Deck Name set during generation.") . deckName)
        -- Keeping at one now as this is for the default deck. But i'm not sure what
        -- that meanas. Perhaps we should not have the default deck at all?
        let newDeck =
              colDeckDefault
                { confDeck = Just 1,
                  idDeck = Just dId,
                  modDeck = Just miliEpoc,
                  nameDeck = Just dName
                }

        [[decksResult :: T.Text]] <- liftIO $ query_ conn (Query "SELECT decks FROM col")
        liftIO $ print decksResult
        let decks :: Decks = fromJust $ decodeStrictText decksResult
        let appendedDecks = AKM.insert (AK.fromText $ T.pack (show dId)) newDeck decks
        liftIO $ execute conn (Query "UPDATE col SET decks = ?") (Only $ encodeToLazyText appendedDecks)
        generateDeck conn modelKeys renderedCards

handleCol :: [DeckFile] -> IO ()
handleCol deckFiles = do
  c <- createCollectionDb
  -- Due for a refactor. The only thing we need at the collection level is the media
  let genInfo =
        DGInfo
          { filePath = Nothing,
            deckFileName = Nothing,
            deckName = Nothing,
            deckId = Nothing,
            mediaDG = []
          }
  modelKeys <- setupCollectionDb c
  finalState <- -- need to do this to accumulate our media
    foldM
      ( \st deck -> do
          (_, newSt) <- runStateT (handleDeck c modelKeys deck) st
          return newSt
      )
      genInfo
      deckFiles
  print finalState
  -- let colConf = undefined -- get from col db
  -- let dId = undefined -- get tail deck id from col db
  -- let colConf' = colConf {activeDecksConf = Just [dId]}
  -- update colConf
  writeDbToApkg finalState

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

  inputSources <- mapM makeAbsolute [source | SourcePath source <- args]
  let _opts = [arg | arg <- args, (case arg of SourcePath _ -> False; _nonFileArg -> True)]
  trees <- mapM (`constructDeckTree` []) inputSources
  mapM_ handleCol [head trees]

constructDeckTree :: FilePath -> [String] -> IO [DeckFile]
constructDeckTree path s =
  doesDirectoryExist path
    >>= ( \case
            True -> do
              paths <- listDirectory path
              let deckName =
                    if not (null [p | p <- paths, case p of ('.' : _) -> True; _ -> False])
                      then tail (head paths)
                      else case reverse path of
                        ('/' : rest) -> takeBaseName $ takeDirectory (reverse rest)
                        _nonDirStylePath -> takeBaseName path
              let filesToProcess = [path ++ "/" ++ p | p <- paths, case p of ('.' : _) -> False; _nonSpecialFile -> True]
              let s' = s ++ [deckName]
              deckFiless <- mapM (`constructDeckTree` s') filesToProcess
              return $ concat deckFiless
            False -> return [InputFile path (DPos s)]
        )

newtype DeckPos = DPos [String]

instance Show DeckPos where
  show (DPos xs) = intercalate "::" xs

data DeckFile where
  InputFile :: FilePath -> DeckPos -> DeckFile
  deriving (Show)

data PankyFlag
  = Verbose
  deriving (Show)

data PankyKWarg
  = DeckName
  deriving (Show)

data PankyOption where
  Flag :: PankyFlag -> PankyOption
  Opt :: PankyKWarg -> PankyOption
  deriving (Show)

data PankyArg where
  SourcePath :: FilePath -> PankyArg
  PFlag :: PankyFlag -> PankyArg
  POpt :: PankyKWarg -> String -> PankyArg
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
parseArgs [x] = [SourcePath x]
parseArgs (('-' : optString) : optv : xs) = case parsePankyOption optString of
  Just (Flag flag) -> PFlag flag : parseArgs (optv : xs)
  Just (Opt kwarg) -> POpt kwarg optv : parseArgs xs
  Nothing -> error $ "Invalid CLI arg -" ++ optString
parseArgs (file : xs) = SourcePath file : parseArgs xs
