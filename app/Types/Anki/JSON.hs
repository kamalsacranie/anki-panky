module Types.Anki.JSON
  ( Deck (..),
    Decks,
    Model (..),
    Models,
    MConf (..),
  )
where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    defaultOptions,
    fieldLabelModifier,
    genericParseJSON,
    genericToJSON,
  )
import Data.Aeson.KeyMap (KeyMap)
import Data.Text.Lazy qualified as T (Text)
import GHC.Generics (Generic)

-- We can fix the warnings here by using bytestrings if we don't want to import
-- another dependency. I will test. It depends how much it inflates our binary.
dropSuffix :: String -> String -> String
dropSuffix suffix str = take (length str - length suffix) str

deckSuffix :: String
deckSuffix = "Deck"

-- | The Deck JSON object
data Deck where
  -- | The constructor for a deck
  Deck ::
    { -- | Whether the deck is collapsed
      collapsedDeck :: Bool,
      -- | The ID of a `Types.Anki.JSON.DeckConf`
      confDeck :: Maybe Int,
      -- | The description of the deck
      descDeck :: String,
      -- | Whether the deck is dynamic (A.K.A. filtered) or not. 0 means it is
      -- not
      dynDeck :: Int,
      extendNewDeck :: Int,
      extendRevDeck :: Int,
      -- | The ID of the deck. This is the time in epoch MILISECONDS x 10
      idDeck :: Maybe Int,
      lrnTodayDeck :: [Int],
      -- | The time the deck was last modified in epoch MILISECONDS x 10
      modDeck :: Maybe Int,
      -- | The name of the deck. This is responsible for how your deck is
      -- nested in the collection. Anki separates decks with the "::" character
      -- to show that they have a hierarchy. Hence, a deck with the name
      -- "Deck::Subdeck" will be shown as a subdeck of "Deck"
      nameDeck :: Maybe T.Text,
      newTodayDeck :: [Int],
      revTodayDeck :: [Int],
      timeTodayDeck :: [Int],
      -- | The update sequence number. Used to figure out server changes. -1
      -- means changes have not been uploaded
      usnDeck :: Int
    } ->
    Deck
  deriving (Show, Generic)

instance ToJSON Deck where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = dropSuffix deckSuffix}

instance FromJSON Deck where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = dropSuffix deckSuffix}

-- | A `KeyMap` of decks where the `Key` is the string ID from `Deck`
type Decks = KeyMap Deck

-- | A deck configureation: UNIMPLEMENTED
data DeckConf = DeckConfig {}

-- | A key map of deck configurations where the `Key` is the deck id
type DeckConfs = KeyMap DeckConf

-- Figure out how to apply a function to all the fields of a record. I.e.,
-- unmabe all the fields and cast it to a strict version. Surely there is a way
-- to derive a model where no fields are optional from a model where all fields
-- are optional.

modelSuffix :: String
modelSuffix = "Model"

-- | The Model JSON object
data Model where
  -- | The constructor for a `Model`
  Model ::
    { -- | The CSS for the model as a string
      cssModel :: Maybe T.Text,
      -- | The ID from `Deck` to which cards are added to by default
      didModel :: Maybe Int,
      -- | The fields of the model represented by the `Field` JSON object
      fldsModel :: [Field],
      -- | The ID of the model in epoch MILISECONDS. Something to note here is
      -- that when Anki imports a collection, it adds a new model unless the
      -- model already exists. This means that you will want to have this value
      -- hardcoded
      idModel :: Maybe T.Text,
      -- | Latex post-amble string
      latexPostModel :: Maybe T.Text,
      -- | Latex preamble string
      latexPreModel :: Maybe T.Text,
      latexsvgModel :: Bool,
      -- | The time the model was last modified in SECONDS
      modModel :: Maybe Int,
      -- | The name of the model
      nameModel :: String,
      reqModel :: [(Int, String, [Int])],
      -- | The sort field of the model
      sortfModel :: Int,
      tagsModel :: [String],
      -- | Array of card `Template`s
      tmplsModel :: [Template],
      -- | Specifies what type of model. 0 for standard, 1 for cloze
      typeModel :: Maybe Int,
      -- | Update sequence number. Used to figure out server changes. -1 means
      -- changes have not been uploaded
      usnModel :: Int,
      versModel :: [Int]
    } ->
    Model
  deriving (Show, Generic)

instance ToJSON Model where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = dropSuffix modelSuffix}

instance FromJSON Model where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = dropSuffix modelSuffix}

type Models = KeyMap Model

-- | A Field of a `Model`
data Field where
  -- | The constructor for a `Field`
  Field ::
    { -- | The font of the field. The name of one of the fonts you can pick in
      -- Anki
      fontField :: String,
      -- | The font size of the field
      sizeField :: Int,
      -- | Unused
      mediaField :: [String],
      -- | The name of the field
      nameField :: String,
      -- | The order of the field. I.e., the position of the field in the card
      ordField :: Int,
      -- | Whether the field is RTL (Right to Left)
      rtlField :: Bool,
      -- | Whether the field is sticky (retains the value of the previous added
      -- note)
      stickyField :: Bool
    } ->
    Field
  deriving (Show, Generic)

instance ToJSON Field where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = dropSuffix "Field"}

instance FromJSON Field where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = dropSuffix "Field"}

templateSuffix :: String
templateSuffix = "Template"

data Template where
  Template ::
    { afmtTemplate :: String,
      bafmtTemplate :: String,
      bqfmtTemplate :: String,
      didTemplate :: Maybe Int,
      nameTemplate :: String,
      ordTemplate :: Int,
      qfmtTemplate :: String,
      bfontTemplate :: String,
      bsizeTemplate :: Int
    } ->
    Template
  deriving (Show, Generic)

instance ToJSON Template where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = dropSuffix templateSuffix}

instance FromJSON Template where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = dropSuffix templateSuffix}

data MConf where
  MConf ::
    { activeDecksMConf :: Maybe [Int],
      addToCurMConf :: Bool,
      collapseTimeMConf :: Int,
      curDeckMConf :: Int,
      curModelMConf :: Maybe T.Text,
      dueCountsMConf :: Bool,
      estTimesMConf :: Bool,
      newBuryMConf :: Bool,
      newSpreadMConf :: Int,
      nextPosMConf :: Int,
      sortBackwardsMConf :: Bool,
      sortTypeMConf :: String,
      timeLimMConf :: Int
    } ->
    MConf
  deriving (Show, Generic)

mConfSuffix :: String
mConfSuffix = "MConf"

instance ToJSON MConf where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = dropSuffix mConfSuffix}

instance FromJSON MConf where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = dropSuffix mConfSuffix}
