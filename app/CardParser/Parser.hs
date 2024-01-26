{-# LANGUAGE LambdaCase #-}

module CardParser.Parser where

import Control.Applicative
import Text.Pandoc

data Parser a = P ([Block] -> [(a, [Block])])

instance Functor Parser where
  fmap f p = P (\inp -> map (\(x, out) -> (f x, out)) $ parse p inp)

instance Applicative Parser where
  pure x = P (\inp -> [(x, inp)])
  pg <*> pa = P (\inp -> concat $ map (\(g, out) -> parse (fmap g pa) out) $ parse pg inp)

instance Monad Parser where
  p >>= f = P (\inp -> concat $ map (\(x, out) -> parse (f x) out) (parse p inp))

instance Alternative Parser where
  empty = P (const [])
  p <|> q =
    P
      ( \inp -> case parse p inp of
          [] -> parse q inp
          rs -> rs
      )

parse :: Parser a -> [Block] -> [(a, [Block])]
parse (P f) = f

parseAll :: Parser a -> [Block] -> [a]
parseAll p bs = [a | (a, []) <- parse p bs]

item :: Parser Block
item =
  P
    ( \case
        [] -> []
        (x : xs) -> [(x, xs)]
    )
