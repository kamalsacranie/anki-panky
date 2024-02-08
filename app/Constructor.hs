{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Constructor where

import CardParser.ExtCard (extendedCard)
import CardParser.Parser
import CardParser.SimpleCard
import Control.Applicative
import Control.Monad.Cont (MonadIO (liftIO))
import Control.Monad.State (State, gets, modify, runState)
import Data.Maybe (fromMaybe)
import Data.Set as Set
import Data.Text qualified as StrictT
import Data.Text.Lazy qualified as T
import System.FilePath (takeDirectory, (</>))
import Text.Pandoc hiding (getPOSIXTime)
import Text.Pandoc.Shared (textToIdentifier)
import Types (DeckGenInfo (..), DeckMediaSet, MediaDeck, MediaItem (DeckMedia), Panky, RenderedDeck)
import Types.CLI (DeckPos)
import Types.Parser as P

textToAst :: T.Text -> IO Pandoc
textToAst txt = do
  runIOorExplode $
    readMarkdown def {readerExtensions = pandocExtensions, readerStandalone = True, readerStripComments = True} (T.toStrict txt)

documenttizeDeck :: ([Block] -> Pandoc) -> [P.Card] -> [(Pandoc, Pandoc)]
documenttizeDeck document =
  Prelude.map
    ( \case
        (P.Card (SimpleFront f) fv) -> (document [f], document fv)
        (P.Card (ExtendedFront f) fv) -> (document f, document fv)
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

renderDeck :: [(Pandoc, Pandoc)] -> IO RenderedDeck
renderDeck = mapM single
  where
    single (f, b) = ((,) <$> writeFlashCardHtml f) <*> writeFlashCardHtml b

cards :: Parser [P.Card]
cards = many (extendedCard <|> simpleCard)

metaValueToText :: MetaValue -> Maybe T.Text
metaValueToText (MetaString s) = pure (T.fromStrict s)
metaValueToText (MetaInlines i) = renderMeta [Para i]
metaValueToText (MetaBlocks b) = renderMeta b
metaValueToText _ = Nothing

renderMeta :: [Block] -> Maybe T.Text
renderMeta metaValue =
  let plainTextMeta = runPure $ writePlain def (Pandoc nullMeta metaValue)
   in case plainTextMeta of
        Left _ -> Nothing
        Right res -> Just (T.dropEnd 1 (T.fromStrict res))

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

-- | Returns a list of tuples which contain rendered HTML for the front and
-- | back of the cards provided in the order that they appeared
produceDeck :: Pandoc -> DeckPos -> Panky (RenderedDeck, MediaDeck)
produceDeck (Pandoc meta blocks') dpos = do
  fp <- gets deckPath
  let (blocks, media) = runState (mediaSetFromBlocks blocks' (takeDirectory fp)) Set.empty
  let documentName = lookupMeta "name" meta
  modify
    ( \st -> case documentName of
        Nothing -> st
        Just name -> st {deckName = T.pack (show dpos ++ "::") <> fromMaybe (deckName st) (metaValueToText name)}
    )
  let deck = concat $ parseAll cards blocks
      docDeck = documenttizeDeck (Pandoc meta) deck
  liftIO $ (,zip [0 :: Int ..] (Set.toList media)) <$> renderDeck docDeck
