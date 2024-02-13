{-# LANGUAGE LambdaCase #-}

module Render where

import CardParser.Parser (parseCards)
import Control.Monad.State (State, modify, runState)
import Data.Set qualified as Set
import Data.Text qualified as StrictT
import Data.Text.Lazy qualified as T
import System.FilePath (takeDirectory, (</>))
import Text.Pandoc hiding (getPOSIXTime)
import Text.Pandoc.Shared (textToIdentifier)
import Types (DeckMediaSet, MediaItem (DeckMedia), RenderedCard (RCard), RenderedDeck)
import Types.Parser as P

renderMDtoNative :: T.Text -> IO Pandoc
renderMDtoNative txt = do
  runIOorExplode $
    readMarkdown def {readerExtensions = pandocExtensions, readerStandalone = True, readerStripComments = True} (T.toStrict txt)

documenttizeDeck :: ([Block] -> Pandoc) -> [P.Card] -> [(Pandoc, Pandoc, CardTags)]
documenttizeDeck document =
  map
    ( \case
        (P.Card (SimpleFront f) fv tags) -> (document [f], document fv, tags)
        (P.Card (ExtendedFront f) fv tags) -> (document f, document fv, tags)
    )

writeFlashCardHtml :: Pandoc -> IO T.Text
writeFlashCardHtml p = do
  runIOorExplode $
    writeHtml5LazyString
      ( def
          { writerExtensions = pandocExtensions,
            writerHTMLMathMethod = MathJax defaultMathJaxURL,
            writerTemplate = Nothing
          }
      )
      p
  where
    writeHtml5LazyString wopts doc = T.fromStrict <$> writeHtml5String wopts doc

renderDeck :: [(Pandoc, Pandoc, CardTags)] -> IO RenderedDeck
renderDeck = mapM single
  where
    single (f, b, tags) = RCard <$> writeFlashCardHtml f <*> writeFlashCardHtml b <*> pure tags

mediaSetFromBlocks :: [Block] -> FilePath -> State DeckMediaSet [Block]
mediaSetFromBlocks [] _ = return []
mediaSetFromBlocks ((Figure a b [Plain [Image c d (url, e)]]) : xs) root = do
  let internalRep = textToIdentifier emptyExtensions url
  let fullUrl = root </> StrictT.unpack url
  modify (Set.insert (DeckMedia fullUrl (T.fromStrict internalRep)))
  blocks <- mediaSetFromBlocks xs root
  return (Figure a b [Plain [Image c d (internalRep, e)]] : blocks)
mediaSetFromBlocks (x : xs) root = do
  blocks <- mediaSetFromBlocks xs root
  return (x : blocks)

renderPandocAsDecks :: Pandoc -> IO RenderedDeck
renderPandocAsDecks (Pandoc meta blocks) = do
  let deck = parseCards blocks
      pandocAstDeck = documenttizeDeck (Pandoc meta) deck
  renderDeck pandocAstDeck

normaliseAndExtractMedia :: Pandoc -> FilePath -> IO (Pandoc, [MediaItem])
normaliseAndExtractMedia (Pandoc meta blocks) fp = do
  let (normalisedBlocks, media) = runState (mediaSetFromBlocks blocks (takeDirectory fp)) Set.empty
  return (Pandoc meta normalisedBlocks, Set.toList media)
