{-# LANGUAGE TemplateHaskell #-}

module Utils
  ( splitListOnce,
    todo,
  )
where

import Language.Haskell.TH

-- | Split a list into two parts at the first occurrence of an element,
-- discarding the element specified to split on.
splitListOnce :: (Eq a) => a -> [a] -> [[a]]
splitListOnce _ [] = []
splitListOnce delimiter str =
  let (first, remainder) = break (== delimiter) str
   in first : case remainder of
        (_ : rest) -> [rest]
        [] -> []

todo :: String -> Q Exp
todo msg = do
  loc <- location
  let pos = formatLoc loc
  [| error ("TODO at " ++ pos ++ " - " ++ msg) |]
  where
  formatLoc loc =
    let (line, col) = loc_start loc
        file = loc_filename loc
    in file ++ ":" ++ show line ++ ":" ++ show (col + 1) -- add 1 to col (0-based)
