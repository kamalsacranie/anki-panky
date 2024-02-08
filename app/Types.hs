module Types
  ( DeckGenInfo (..),
    DeckMediaSet,
    Panky,
    MediaItem (..),
  )
where

import Control.Monad.State.Lazy (StateT)
import Data.Set as Set (Set)
import Data.Text.Lazy qualified as T

data MediaItem where
  DeckMedia :: FilePath -> T.Text -> MediaItem
  deriving (Show, Eq, Ord)

type DeckMediaSet = Set.Set MediaItem

type Panky a = StateT DeckGenInfo IO a

data DeckGenInfo where
  DGInfo ::
    { deckName :: Maybe T.Text,
      filePath :: Maybe FilePath,
      deckFileName :: Maybe String,
      deckId :: Maybe Int,
      mediaDG :: [(Int, MediaItem)]
    } ->
    DeckGenInfo
  deriving (Show)
