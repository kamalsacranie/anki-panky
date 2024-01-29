{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Temp where

-- import Control.Applicative (empty)
import Data.Aeson
import Data.Aeson.KeyMap (KeyMap)
import GHC.Generics
import Prelude.Compat hiding (id, mod)

data Deck = Deck
  { collapsed :: Bool,
    conf :: Int,
    desc :: String,
    dyn :: Int,
    extendNew :: Int,
    extendRev :: Int,
    id :: Int,
    lrnToday :: [Int],
    mod :: Int,
    name :: String,
    newToday :: [Int],
    revToday :: [Int],
    timeToday :: [Int],
    usn :: Int
  }
  deriving (Show, Generic, ToJSON, FromJSON)

type DeckList = KeyMap Deck

-- instance ToJSON DeckList

-- instance FromJSON DeckList

-- instance ToJSON Deck where
--   toJSON Deck {..} =
--     object
--       [ "collapsed" .= collapsed,
--         "conf" .= conf,
--         "desc" .= desc,
--         "dyn" .= dyn,
--         "extendNew" .= extendNew,
--         "extendRev" .= extendRev,
--         "id" .= id,
--         "lrnToday" .= lrnToday,
--         "mod" .= mod,
--         "name" .= name,
--         "newToday" .= newToday,
--         "revToday" .= revToday,
--         "timeToday" .= timeToday,
--         "usn" .= usn
--       ]

--   toEncoding Deck {..} =
--     pairs $
--       "collapsed" .= collapsed
--         <> "conf" .= conf
--         <> "desc" .= desc
--         <> "dyn" .= dyn
--         <> "extendNew" .= extendNew
--         <> "extendRev" .= extendRev
--         <> "id" .= id
--         <> "lrnToday" .= lrnToday
--         <> "mod" .= mod
--         <> "name" .= name
--         <> "newToday" .= newToday
--         <> "revToday" .= revToday
--         <> "timeToday" .= timeToday
--         <> "usn" .= usn

-- instance FromJSON Deck where
--   parseJSON (Object v) =
--     Deck
--       <$> v .: "collapsed"
--       <*> v .: "conf"
--       <*> v .: "desc"
--       <*> v .: "dyn"
--       <*> v .: "extendNew"
--       <*> v .: "extendRev"
--       <*> v .: "id"
--       <*> v .: "lrnToday"
--       <*> v .: "mod"
--       <*> v .: "name"
--       <*> v .: "newToday"
--       <*> v .: "revToday"
--       <*> v .: "timeToday"
--       <*> v .: "usn"
--   parseJSON _ = empty
