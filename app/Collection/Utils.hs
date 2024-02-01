module Collection.Utils where

import Control.Monad (when)
import System.Directory (doesFileExist, removeFile)

-- {-# LANGUAGE OverloadedStrings #-}
-- import Data.List (intercalate)

-- base91Table :: [Char]
-- base91Table = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "!#$%&()*+,-./:;<=>?@[\\]^_`{|}~"

-- genGUID :: [String] -> IO String
-- genGUID vals = do
--   let t = intercalate "__" vals

--   return ""

removeIfExists :: FilePath -> IO ()
removeIfExists filePath = do
  exists <- doesFileExist filePath
  when exists $ removeFile filePath
