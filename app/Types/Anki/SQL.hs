module Types.Anki.SQL
  ( Col (..),
    Note (..),
    Card (..),
  )
where

import Data.Text.Lazy qualified as T (Text)
import Database.SQLite.Simple (FromRow, ToRow)
import GHC.Generics

-- | A SQL Anki collection table
-- All fieds are mandatory
data Col where
  -- | Collection type constructor
  Col ::
    { -- | The ID of the collection. This is alwys 1 as we will only ever have
      -- one collection
      idCol :: !Int,
      -- | The time in of the creation of the collection in epoch SECONDS
      crtCol :: !Int,
      -- | The time the collection was last modified in epoch MILISECONDS
      modCol :: !Int,
      -- | The time the schema of the collection was last modified in epoch MILISECONDS
      scmCol :: !Int,
      -- | Anki version number of the collection. 11 is the most recent at the
      -- time of writing
      verCol :: !Int,
      -- | A flag to indicate if the collection is dirty. 0 means it is clean
      -- (i.e. has not been used)
      dtyCol :: !Int,
      -- | Update sequence number. Used to figure out server changes. -1 means
      -- changes have not been uploaded
      usnCol :: !Int,
      -- | Last time the collection was synced. 0 means never
      lsCol :: !Int,
      -- | The collections configuration. Text JSON of the type
      -- `Types.Anki.JSON.MConf`
      confCol :: !T.Text,
      -- | The models which are used in the collection. T.Text JSON of the type
      -- `Types.Anki.JSON.Models`
      modelsCol :: !T.Text,
      -- | The decks which are used in the collection. T.Text JSON of the type
      -- `Types.Anki.JSON.Decks`
      decksCol :: !T.Text,
      -- | The deck configurations which are used in the collection. T.Text JSON
      -- of the type `Types.Anki.JSON.DeckConfs`
      dconfCol :: !T.Text,
      -- | Currently unused. Should take the value of "{}"
      tagsCol :: !T.Text
    } ->
    Col
  deriving (Show, Generic, FromRow, ToRow)

-- | A SQL Anki note table
-- All fields are mandatory
data Note where
  -- | Note type constructor
  Note ::
    { -- | The ID of the note. This is the time in epoch MILISECONDS x 10. This
      -- is contrary to Anki's internal representation but otherwise notes are
      -- generated too quickly and we do not satisfy the unique note.id
      -- constraint in our database.
      idNote :: !Int,
      -- | The global unique ID of the note, used to prevent notes from being
      -- duplicated when re-importing collections with the same notes. If the
      -- fields of the note remain unchages, the GUID should remain the same.
      guidNote :: !T.Text,
      -- | The ID of the `Types.Anki.JSON.Model` that the note uses.
      midNote :: !Int,
      -- | The time in of the modification in epoch SECONDS
      modNote :: !Int,
      -- | Update sequence number. Used to figure out server changes. -1 means
      -- changes have not been uploaded
      usnNote :: !Int,
      -- | A space separated list of tags related to the note
      tagsNote :: !T.Text,
      -- | The fields of the note. T.Text JSON of the type `Types.Anki.JSON.Fields`
      fldsNote :: !T.Text,
      -- | The note's sort field. This is used to sort the notes in Anki.
      -- Anki's database has this as an integer but we mark it as a string so
      -- that the field can be sorted.
      sfldNote :: !T.Text,
      -- | The note's checksum. Used for duplicate check. It is an integer
      -- representation of first 8 digits of sha1 hash of the first field.
      csumNote :: !Int,
      -- | The note's flags. Currently unused. Should take the value of 0
      flagsNote :: !Int,
      -- | Currently unused. Should take the value of ""
      dataNote :: !T.Text
    } ->
    Note
  deriving (Show, Generic, FromRow, ToRow)

-- | A SQL Anki card table
-- All fields are mandatory
data Card where
  -- | Card type constructor
  Card ::
    { -- | The ID of the card. This is the time in epoch MILISECONDS x 10. This
      -- is contrary to Anki's internal representation but otherwise notes are
      -- generated too quickly and we do not satisfy the unique note.id
      -- constraint in our database.
      idCard :: !Int,
      -- | The ID of the `Note` that the card is associated with
      nidCard :: !Int,
      -- | The ID of the `Types.Anki.JSON.Deck` that the card is associated with
      didCard :: !Int,
      -- | The ordinal of the card. identifies which of the card templates or
      -- cloze deletions it corresponds to:
      --   for card templates, valid values are from 0 to num templates - 1;
      --   for cloze deletions, valid values are from 0 to max cloze index - 1
      ordCard :: !Int,
      -- | The time the card was last modified in epoch SECONDS
      modCard :: !Int,
      -- | Update sequence number. Used to figure out server changes. -1 means
      -- changes have not been uploaded
      usnCard :: !Int,
      -- | The type of the card. 0 means new, 1 means learning, 2 means review,
      -- 3 means relearning
      typeCard :: !Int,
      -- | -3: Card buried by user,
      -- -2: Card buried by scheduler,
      -- -1: Card suspended,
      -- 0: new, 1: learning, 2: review (as for type)
      -- 3: in learning, next rev in at least a day after the previous review
      -- 4: preview
      queueCard :: !Int,
      -- | Due is used differently for different card types:
      --   new: note id or random int
      --   due: integer day, relative to the collection's creation time
      --   learning: integer timestamp in second
      dueCard :: !Int,
      -- | Interval until the next review. A negative int represents seconds and a
      -- positive int represents days
      ivlCard :: !Int,
      -- | The ease factor of the card in permille (parts per thousand). If the
      -- ease factor is 2500, the card’s interval will be multiplied by 2.5 the
      -- next time you press Good.
      factorCard :: !Int,
      -- | The number of reviews the card has had
      repsCard :: !Int,
      -- | the number of times the card went from a "was answered correctly" to
      -- "was answered incorrectly" state
      lapsesCard :: !Int,
      -- | Of the form a*1000+b, with:
      -- a the number of reps left today
      -- b the number of reps left till graduation
      -- for example: '2004' means 2 reps left today and 4 reps till graduation
      leftCard :: !Int,
      -- | original due: In filtered decks, it's the original due date that the
      -- card had before moving to filtered.
      -- If the card lapsed in scheduler 1, then it's the value before the
      -- lapse. (This is used when switching to scheduler 2. At this time,
      -- cards in learning becomes due again, with their previous due date)
      -- In any other case it's 0.
      odueCard :: !Int,
      -- | Original did: only used when the card is currently in filtered deck
      odidCard :: !Int,
      -- | an integer. This integer mod 8 represents a "flag", which can be seen
      -- in browser and while reviewing a note. Red 1, Orange 2, Green 3, Blue 4,
      -- no flag: 0. This integer divided by 8 represents currently nothing
      flagsCard :: !Int,
      -- Currently unused. Should take the value of ""
      dataCard :: !T.Text
    } ->
    Card
  deriving (Show, Generic, FromRow, ToRow)
