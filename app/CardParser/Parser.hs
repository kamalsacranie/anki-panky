{-# LANGUAGE LambdaCase #-}

module CardParser.Parser where

import Text.Pandoc
import Types.Parser

parseAll :: Parser a -> [Block] -> [a]
parseAll p bs = [a | (a, []) <- parse p bs]

item :: Parser Block
item =
  P
    ( \case
        [] -> []
        (x : xs) -> [(x, xs)]
    )
