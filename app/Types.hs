module Types where

import Text.Pandoc

data Front
  = SimpleFront Block
  | ExtendedFront [Block]
  deriving (Show)

type Back = [Block]

data Card
  = Card Front Back
  deriving (Show)
