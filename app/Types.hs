module Types
  ( DeckGenInfo (..),
    DeckMediaSet,
    Panky,
    MediaItem (..),
    RenderedDeck,
    MediaDeck,
  )
where

import Control.Monad.State.Lazy (StateT)
import Data.Set as Set (Set)
import Data.Text.Lazy qualified as T

data MediaItem where
  DeckMedia :: FilePath -> T.Text -> MediaItem
  deriving (Show, Eq, Ord)

type DeckMediaSet = Set.Set MediaItem

type MediaDeck = [(Int, MediaItem)]

type Panky a = StateT DeckGenInfo IO a

type RenderedDeck = [(T.Text, T.Text)]

data DeckGenInfo where
  DGInfo ::
    { deckName :: T.Text,
      deckPath :: FilePath,
      deckFileName :: String,
      deckId :: Int
    } ->
    DeckGenInfo
  deriving (Show)
