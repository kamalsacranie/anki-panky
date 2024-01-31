{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types.Anki where

import Data.Aeson
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
  { idNote :: !Int,
    guidNote :: !Text,
    midNote :: !Int,
    modNote :: !Int,
    usnNote :: !Int,
    tagsNote :: !Text,
    fldsNote :: !Text,
    sfldNote :: !Text, -- This is actually an integer in the database but we mark it as such so that the field can be sorted
    csumNote :: !Int,
    flagsNote :: !Int,
    dataNote :: !Text
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

--------- JSON DEFINITIONS -----------------------------------------------------

dropSuffix :: String -> String -> String
dropSuffix suffix str = take (length str - length suffix) str

deckSuffix :: String
deckSuffix = "Deck"

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
  deriving (Show, Generic)

instance ToJSON Deck where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = dropSuffix deckSuffix}

instance FromJSON Deck where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = dropSuffix deckSuffix}

-- This is how we denote an arbitrary key to value json object
type Decks = KeyMap Deck

data Model = Model
  { cssModel :: String,
    didModel :: Int,
    fldsModel :: [Field],
    idModel :: String,
    latexPostModel :: String,
    latexPreModel :: String,
    latexsvgModel :: Bool,
    modModel :: Int,
    nameModel :: String,
    reqModel :: [(Int, String, [Int])], -- not really used anymore and CBA to implement for backward compat
    sortfModel :: Int,
    tagsModel :: [String],
    tmplsModel :: [Template],
    typeModel :: Int,
    usnModel :: Int,
    versModel :: [Int]
  }
  deriving (Show, Generic)

instance ToJSON Model where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = dropSuffix "Model"}

instance FromJSON Model where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = dropSuffix "Model"}

type Models = KeyMap Model

data Field = Field
  { fontField :: String,
    mediaField :: [String],
    nameField :: String,
    ordField :: Int,
    rtlField :: Bool,
    sizeField :: Int,
    stickyField :: Bool
  }
  deriving (Show, Generic)

instance ToJSON Field where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = dropSuffix "Field"}

instance FromJSON Field where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = dropSuffix "Field"}

data Template = Template
  { afmtTemplate :: String,
    bafmtTemplate :: String,
    bqfmtTemplate :: String,
    didTemplate :: Maybe Int,
    nameTemplate :: String,
    ordTemplate :: Int,
    qfmtTemplate :: String,
    bfontTemplate :: String,
    bsizeTemplate :: Int
  }
  deriving (Show, Generic)

instance ToJSON Template where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = dropSuffix "Template"}

instance FromJSON Template where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = dropSuffix "Template"}
