{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types.Anki where

import Control.Monad.State.Lazy (StateT)
import Data.Aeson
import Data.Aeson.KeyMap (KeyMap)
import Data.Set as Set
import Data.Text as T
import Database.SQLite.Simple (FromRow, ToRow)
import GHC.Generics

data DeckGenInfo = DGInfo
  { deckName :: T.Text,
    filePath :: FilePath,
    deckFileName :: String,
    deckId :: Maybe Int,
    mediaDG :: [(Int, MediaItem)]
  }
  deriving (Show)

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
dropSuffix suffix str = Prelude.take (Prelude.length str - Prelude.length suffix) str

deckSuffix :: String
deckSuffix = "Deck"

data MediaItem = DeckMedia FilePath String deriving (Show, Eq, Ord)

type DeckMediaSet = Set.Set MediaItem

type DeckMedia = [(Int, MediaItem)]

type Panky a = StateT DeckGenInfo IO a

data Deck = Deck
  { collapsedDeck :: Bool,
    confDeck :: Maybe Int,
    descDeck :: String,
    dynDeck :: Int,
    extendNewDeck :: Int,
    extendRevDeck :: Int,
    idDeck :: Maybe Int,
    lrnTodayDeck :: [Int],
    modDeck :: Maybe Int,
    nameDeck :: Maybe T.Text,
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

-- Figure out how to apply a function to all the fields of a record
data Model = Model
  { cssModel :: Maybe T.Text,
    didModel :: Maybe Int,
    fldsModel :: [Field],
    idModel :: Maybe T.Text,
    latexPostModel :: Maybe T.Text,
    latexPreModel :: Maybe T.Text,
    latexsvgModel :: Bool,
    modModel :: Maybe Int,
    nameModel :: String,
    reqModel :: [(Int, String, [Int])],
    sortfModel :: Int,
    tagsModel :: [String],
    tmplsModel :: [Template],
    typeModel :: Maybe Int,
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

data Conf = Conf
  { activeDecksConf :: Maybe [Int],
    addToCurConf :: Bool,
    collapseTimeConf :: Int,
    curDeckConf :: Int,
    curModelConf :: Maybe T.Text,
    dueCountsConf :: Bool,
    estTimesConf :: Bool,
    newBuryConf :: Bool,
    newSpreadConf :: Int,
    nextPosConf :: Int,
    sortBackwardsConf :: Bool,
    sortTypeConf :: String,
    timeLimConf :: Int
  }
  deriving (Show, Generic)

instance ToJSON Conf where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = dropSuffix "Conf"}

instance FromJSON Conf where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = dropSuffix "Conf"}
