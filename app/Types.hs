{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Text as TT
import Database.SQLite.Simple
import GHC.Generics
import Text.Pandoc

data Front
  = SimpleFront Block
  | ExtendedFront [Block]
  deriving (Show)

type Back = [Block]

data Card
  = Card Front Back
  deriving (Show)

data DBCard = DBCard
  { idCard :: Int,
    nidCard :: Int,
    didCard :: Int,
    ordCard :: Int,
    modCard :: Int,
    usnCard :: Int,
    typeCard :: Int,
    queueCard :: Int,
    dueCard :: Int,
    ivlCard :: Int,
    factorCard :: Int,
    repsCard :: Int,
    lapsesCard :: Int,
    leftCard :: Int,
    odueCard :: Int,
    odidCard :: Int,
    flagsCard :: Int,
    dataCard :: TT.Text
  }
  deriving (Show, Generic, FromRow, ToRow)
