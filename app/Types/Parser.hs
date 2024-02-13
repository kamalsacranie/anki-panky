module Types.Parser where

import Control.Applicative
import Text.Pandoc

data Parser a where
  P :: ([Block] -> [(a, [Block])]) -> Parser a

parse :: Parser a -> [Block] -> [(a, [Block])]
parse (P f) = f

instance Functor Parser where
  fmap f p = P (\inp -> map (\(x, out) -> (f x, out)) $ parse p inp)

instance Applicative Parser where
  pure x = P (\inp -> [(x, inp)])
  pg <*> pa =
    P
      ( \inp ->
          concat $ map (\(g, out) -> parse (fmap g pa) out) $ parse pg inp
      )

instance Monad Parser where
  p >>= f =
    P
      ( \inp ->
          concat $ map (\(x, out) -> parse (f x) out) (parse p inp)
      )

instance Alternative Parser where
  empty = P (const [])
  p <|> q =
    P
      ( \inp -> case parse p inp of
          [] -> parse q inp
          rs -> rs
      )

-- | A front of a card as an AST representation
data Front where
  -- | A simple font has only a H1 object as the question
  SimpleFront :: Block -> Front
  -- | An extended front is either started by having a ". . ." after an H1, or
  -- it is denoted by "---" to denote the start of a new card in place of an H1
  ExtendedFront :: [Block] -> Front
  deriving (Show)

-- | A back is just all the remaining text up until the next H1 or end of the
-- file
type Back = [Block]

newtype CardTags where
  CTags :: [String] -> CardTags

instance Show CardTags where
  show (CTags tags) = unwords tags

-- | An abstract representation of a card
data Card where
  -- | A card is just a front and a back
  Card :: Front -> Back -> CardTags -> Card
  deriving (Show)
