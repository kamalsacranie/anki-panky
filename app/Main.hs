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
import Data.Maybe (fromJust)
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Encoding (decodeUtf8')
import Data.Text.Lazy.IO qualified as LTO (readFile)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.SQLite.Simple (Connection, Only (Only), Query (Query), execute, query_)
import System.Directory (doesDirectoryExist, listDirectory, makeAbsolute)
import System.Environment (getArgs)
import System.FilePath (takeBaseName, takeDirectory)
import Types (DeckGenInfo (..), MediaDeck, MediaItem)
import Types.Anki.JSON (Deck (..), Decks)
import Types.CLI

-- | Checks if the input file is a valid deck file
-- | TODO: Change this implementation to handle an IO exception with readFile from Lazy Text
isValidFile :: BL.ByteString -> Bool
isValidFile input = case decodeUtf8' input of
  Left _ -> False
  Right res -> case T.unpack res of
    ('-' : '-' : '-' : '\n' : _) -> True
    ('#' : ' ' : _) -> True
    _anyOtherFirstLine -> False

handleDeck :: Connection -> [Int] -> DeckFile -> IO [MediaItem]
handleDeck conn modelKeys (InputFile path deckPrefix) = do
  byteTestInput <- BL.take 1000 <$> BL.readFile path
  if isValidFile byteTestInput
    then do
      miliEpoc :: Int <- floor . (* 10000) <$> getPOSIXTime
      let genInfo1 =
            DGInfo
              { deckPath = path,
                deckFileName = takeBaseName path,
                deckName = T.pack (show deckPrefix ++ "::" ++ takeBaseName path),
                deckId = miliEpoc
              }
      input <- LTO.readFile path
      doc <- textToAst input
      ((renderedDeck, mediaFiles), genInfo2) <- runStateT (produceDeck doc deckPrefix) genInfo1
      if null renderedDeck
        then error ("Skipping file \"" ++ path ++ "\" as it failed to parse its cards")
        else do
          colDeckDefault <- fromJust <$> (decodeFileStrict "./data/default-anki-json/deck.json" :: IO (Maybe Deck))
          let newDeck =
                colDeckDefault
                  { confDeck = Just 1,
                    idDeck = Just (deckId genInfo2),
                    modDeck = Just miliEpoc,
                    nameDeck = Just (deckName genInfo2)
                  }

          print $ "Deck name: " ++ show (deckName genInfo2)
          [[decksResult :: T.Text]] <- query_ conn (Query "SELECT decks FROM col")
          let decks :: Decks = fromJust $ decodeStrictText $ T.toStrict decksResult
              appendedDecks = AKM.insert (AK.fromString (show (deckId genInfo2))) newDeck decks
          execute conn (Query "UPDATE col SET decks = ?") (Only $ encodeToLazyText appendedDecks)
          _ <- runStateT (generateDeck conn modelKeys renderedDeck) genInfo2
          return mediaFiles
    else return []

handleCol :: [DeckFile] -> T.Text -> IO ()
handleCol deckFiles colName = do
  c <- createCollectionDb
  modelKeys <- setupCollectionDb c
  mediaFiles <- foldM (\mfiles deck -> (mfiles ++) <$> handleDeck c modelKeys deck) [] deckFiles
  let mediaDeck :: MediaDeck = zip [0 :: Int ..] mediaFiles
  print mediaDeck
  writeDbToApkg mediaDeck colName

takeBasePathName :: FilePath -> String
takeBasePathName path = case reverse path of
  ('/' : rest) -> takeBaseName $ takeDirectory (reverse rest)
  _nonDirStylePath -> takeBaseName path

constructDeckTree :: FilePath -> [T.Text] -> IO [DeckFile]
constructDeckTree path s =
  doesDirectoryExist path
    >>= ( \case
            True -> do
              paths <- listDirectory path
              let deckName =
                    if not (null [p | p <- paths, case p of ('.' : _) -> True; _nonSpecial -> False])
                      then tail (head paths)
                      else takeBasePathName path
              let filesToProcess = [path ++ "/" ++ p | p <- paths, case p of ('.' : _) -> False; _nonSpecialFile -> True]
              let s' = s ++ [T.pack deckName]
              deckFiless <- mapM (`constructDeckTree` s') filesToProcess
              return $ concat deckFiless
            False -> return [InputFile path (DPos s)]
        )

splitStringOnce :: Char -> String -> [String]
splitStringOnce _ "" = []
splitStringOnce delimiter str =
  let (first, remainder) = break (== delimiter) str
   in first : case remainder of
        (_ : rest) -> [rest]
        [] -> []

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
  Just (Opt kwarg) -> POpt kwarg (T.pack optv) : parseArgs xs
  Nothing -> error $ "Invalid CLI arg -" ++ optString
parseArgs (file : xs) = SourcePath file : parseArgs xs

main :: IO ()
main = do
  rawArgs <- concatMap (splitStringOnce '=') <$> getArgs
  let args = parseArgs rawArgs

  inputSources <- mapM makeAbsolute [source | SourcePath source <- args]
  let _opts = [arg | arg <- args, (case arg of SourcePath _ -> False; _nonFileArg -> True)]
  trees <- mapM (\source -> ColDir source <$> constructDeckTree source []) inputSources
  when (null trees) $ error "No input files found"
  mapM_
    ( \case
        ColDir fp [] -> print $ "Skipping invalid input file: " ++ show fp
        ColDir path cds ->
          handleCol cds (T.pack (takeBasePathName path))
    )
    [head trees]
