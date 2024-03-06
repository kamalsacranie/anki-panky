module Types
  ( DeckGenInfo (..),
    DeckMediaSet,
    MediaItem (..),
    RenderedDeck,
    MediaDeck,
    RenderedCard (RCard),
    PankyApp,
    PankyDeck
  )
where

import Control.Monad.State.Lazy (StateT)
import Data.Set as Set (Set)
import Data.Text.Lazy qualified as T
import Types.Parser (CardTags)
import Types.CLI (PankyConfig)

data MediaItem where
  DeckMedia :: FilePath -> T.Text -> MediaItem
  deriving (Show, Eq, Ord)

type DeckMediaSet = Set.Set MediaItem

type MediaDeck = [(Int, MediaItem)]

type PankyDeck a = StateT DeckGenInfo IO a

data RenderedCard where
  RCard :: T.Text -> T.Text -> CardTags -> RenderedCard

type RenderedDeck = [RenderedCard]

data DeckGenInfo where
  DGInfo ::
    { deckName :: T.Text,
      deckPath :: FilePath,
      deckFileName :: String,
      deckId :: Int
    } ->
    DeckGenInfo
  deriving (Show)

type PankyApp a = StateT PankyConfig IO a
