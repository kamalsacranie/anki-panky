module Types.CLI
  ( DeckFile (InputFile),
    PankyArg (SourcePath, PFlag, POpt),
    PankyOption (Flag, Opt),
    DeckPos (DPos),
    PankyFlag (Verbose, Version),
    PankyKWarg (DeckName, OutputDir),
    CollectionDir (ColDir),
    PankyConfig (PankyConfig, outputDirPConf),
  )
where

import Data.Text.Lazy qualified as T
  ( Text,
    intercalate,
    pack,
    unpack,
  )

-- | The location of the deck in the collection given as a list of `Text`.
-- The show method for this types displays an `intercalate`d version of the
-- `Text` where each element is separated by `::`.
newtype DeckPos = DPos [T.Text]

-- | Intercalated representation of deck position separated by `::`.
instance Show DeckPos where
  show (DPos xs) =
    let prefix = T.unpack $ T.intercalate (T.pack "::") xs
     in if null prefix then "" else prefix ++ "::"

-- | A file object to be processed as a deck.
data DeckFile where
  -- | Contains the file path of the file to be processed and the position of
  -- the resulting deck in the collection.
  InputFile :: FilePath -> DeckPos -> DeckFile
  deriving (Show)

data CollectionDir where
  ColDir :: FilePath -> [DeckFile] -> CollectionDir
  deriving (Show)

-- | A flag argument passed to anki-panky from the command line.
data PankyFlag
  = Verbose
  | Version
  deriving (Eq, Show)

-- | A keyword argument passed to anki-panky from the command line.
data PankyKWarg
  = DeckName
  | OutputDir
  deriving (Eq, Show)

-- | Used as a type to distinguish between flags and keyword arguments.
data PankyOption where
  Flag :: PankyFlag -> PankyOption
  Opt :: PankyKWarg -> PankyOption
  deriving (Show)

-- | An argument passed to anki-panky from the command line.
data PankyArg where
  -- | A `FilePath` of a source file passed via the command line
  SourcePath :: FilePath -> PankyArg
  -- | A `PankyFlag` representing a flag argument passed via the command line
  PFlag :: PankyFlag -> PankyArg
  -- | A `PankyKWarg` representing a keyword argument passed via the command
  -- line
  POpt :: PankyKWarg -> T.Text -> PankyArg
  deriving (Eq, Show)

data PankyConfig = PankyConfig
  { outputDirPConf :: FilePath
  }
