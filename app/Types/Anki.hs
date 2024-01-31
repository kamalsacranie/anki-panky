{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types.Anki where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.KeyMap (KeyMap)
import Data.Text (Text)
import Database.SQLite.Simple (FromRow, ToRow)
import GHC.Generics (Generic)

data Col = Col
  { idCol :: !Int,
    crtCol :: !Int,
    modCol :: !Int,
    scmCol :: !Int,
    verCol :: !Int,
    dtyCol :: !Int,
    usnCol :: !Int,
    lsCol :: !Int,
    confCol :: !Text,
    modelsCol :: !Text,
    decksCol :: !Text,
    dconfCol :: !Text,
    tagsCol :: !Text
  }
  deriving (Show, Generic, FromRow, ToRow)

data Note = Note
  { idNote :: Int,
    guidNote :: Text,
    midNote :: Int,
    modNote :: Int,
    usnNote :: Int,
    tagsNote :: Text,
    fldsNote :: Text,
    sfldNote :: Text, -- This is actually an integer in the database but we mark it as such so that the field can be sorted
    csumNote :: Int,
    flagsNote :: Int,
    dataNote :: Text
  }
  deriving (Show, Generic, FromRow, ToRow)

data Card = Card
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
    dataCard :: Text
  }
  deriving (Show, Generic, FromRow, ToRow)

-- data Col = Col Int Int Int Int Int Int Int Int TT.Text TT.Text ByteString TT.Text TT.Text deriving (Show, Generic, FromRow)

-- instance FromRow Col where
--   fromRow =
--     Col
--       <$> field
--       <*> field
--       <*> field
--       <*> field
--       <*> field
--       <*> field
--       <*> field
--       <*> field
--       <*> field
--       <*> field
--       <*> field
--       <*> field
--       <*> field

--------- JSON DEFINITIONS -----------------------------------------------------

data Deck = Deck
  { collapsedDeck :: Bool,
    confDeck :: Int,
    descDeck :: String,
    dynDeck :: Int,
    extendNewDeck :: Int,
    extendRevDeck :: Int,
    idDeck :: Int,
    lrnTodayDeck :: [Int],
    modDeck :: Int,
    nameDeck :: String,
    newTodayDeck :: [Int],
    revTodayDeck :: [Int],
    timeTodayDeck :: [Int],
    usnDeck :: Int
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- This is how we denote an arbitrary key to value json object
type DeckList = KeyMap Deck

-- instance ToJSON DeckList

-- instance FromJSON DeckList
-- {-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE NoImplicitPrelude #-}

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
