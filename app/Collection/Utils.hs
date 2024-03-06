{-# LANGUAGE OverloadedStrings #-}

module Collection.Utils (removeIfExists, genNoteGuid, handleMeta) where

import Control.Monad (when)
import Control.Monad.RWS (modify)
import Crypto.Hash.SHA256 (hash)
import Data.ByteString qualified as BS (ByteString, take)
import Data.ByteString.Char8 qualified as BSC (foldl', pack)
import Data.Char (ord)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Text.Lazy qualified as T
import System.Directory (doesFileExist, removeFile)
import Text.Pandoc hiding (getPOSIXTime)
import Types (DeckGenInfo (deckName), PankyDeck)
import Types.CLI (DeckPos)

metaValueToText :: MetaValue -> Maybe T.Text
metaValueToText (MetaString s) = pure (T.fromStrict s)
metaValueToText (MetaInlines i) = renderMeta [Para i]
metaValueToText (MetaBlocks b) = renderMeta b
metaValueToText _ = Nothing

renderMeta :: [Block] -> Maybe T.Text
renderMeta metaValue =
  let plainTextMeta = runPure $ writePlain def (Pandoc nullMeta metaValue)
   in case plainTextMeta of
        Left _ -> Nothing
        Right res -> Just (T.dropEnd 1 (T.fromStrict res))

handleMeta :: Pandoc -> DeckPos -> PankyDeck ()
handleMeta (Pandoc meta _) dpos = do
  let documentName = lookupMeta "name" meta
  modify
    ( \st -> case documentName of
        Nothing -> st
        Just name -> st {deckName = T.pack (show dpos) <> fromMaybe (deckName st) (metaValueToText name)}
    )

genNoteGuid :: String -> String -> [String] -> String
genNoteGuid x y rest = genNoteGuid' $ BSC.pack (intercalate "__" (x : y : rest))

genNoteGuid' :: BS.ByteString -> String
genNoteGuid' hashStr = toBase91 $ abs (toInt $ BS.take 8 (hash hashStr))

toInt :: BS.ByteString -> Int
toInt = BSC.foldl' (\acc b -> acc * 256 + ord b) 0

toBase91 :: Int -> String
toBase91 n = reverse $ toDigits n []
  where
    toDigits 0 acc = acc
    toDigits x acc = toDigits (x `div` length base91Table) (base91Table !! (x `mod` length base91Table) : acc)

removeIfExists :: FilePath -> IO ()
removeIfExists filePath = do
  exists <- doesFileExist filePath
  when exists $ removeFile filePath

base91Table :: [Char]
base91Table =
  [ 'a',
    'b',
    'c',
    'd',
    'e',
    'f',
    'g',
    'h',
    'i',
    'j',
    'k',
    'l',
    'm',
    'n',
    'o',
    'p',
    'q',
    'r',
    's',
    't',
    'u',
    'v',
    'w',
    'x',
    'y',
    'z',
    'A',
    'B',
    'C',
    'D',
    'E',
    'F',
    'G',
    'H',
    'I',
    'J',
    'K',
    'L',
    'M',
    'N',
    'O',
    'P',
    'Q',
    'R',
    'S',
    'T',
    'U',
    'V',
    'W',
    'X',
    'Y',
    'Z',
    '0',
    '1',
    '2',
    '3',
    '4',
    '5',
    '6',
    '7',
    '8',
    '9',
    '!',
    '#',
    '$',
    '%',
    '&',
    '(',
    ')',
    '*',
    '+',
    ',',
    '-',
    '.',
    '/',
    ':',
    ';',
    '<',
    '=',
    '>',
    '?',
    '@',
    '[',
    ']',
    '^',
    '_',
    '`',
    '{',
    '|',
    '}',
    '~'
  ]
