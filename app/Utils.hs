module Utils
  ( splitListOnce,
  )
where

-- | Split a list into two parts at the first occurrence of an element,
-- discarding the element specified to split on.
splitListOnce :: (Eq a) => a -> [a] -> [[a]]
splitListOnce _ [] = []
splitListOnce delimiter str =
  let (first, remainder) = break (== delimiter) str
   in first : case remainder of
        (_ : rest) -> [rest]
        [] -> []
