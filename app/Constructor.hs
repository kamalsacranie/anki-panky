{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Constructor where

import CardParser.ExtCard (extendedCard)
import CardParser.Parser
import CardParser.SimpleCard
import Control.Applicative
import Data.Text qualified as T
import Text.Pandoc hiding (getPOSIXTime)
import Types.Parser as P

textToAst :: T.Text -> IO Pandoc
textToAst txt = do
  runIOorExplode $
    readMarkdown def {readerExtensions = pandocExtensions, readerStandalone = True} txt

documenttizeDeck :: ([Block] -> Pandoc) -> [P.Card] -> [(Pandoc, Pandoc)]
documenttizeDeck document =
  map
    ( \case
        (P.Card (SimpleFront f) fv) -> (document [f], document fv)
        (P.Card (ExtendedFront f) fv) -> (document f, document fv)
    )

writeFlashCardHtml :: Pandoc -> IO T.Text
writeFlashCardHtml p = do
  -- template <- runIOorExplode $ compileDefaultTemplate "html5"
  runIOorExplode $
    writeHtml5String
      ( def
          { writerExtensions = pandocExtensions,
            writerHTMLMathMethod = MathJax defaultMathJaxURL,
            writerTemplate = Nothing
          }
      )
      p

renderDeck :: [(Pandoc, Pandoc)] -> IO [(T.Text, T.Text)]
renderDeck = mapM single
  where
    single (f, b) = ((,) <$> writeFlashCardHtml f) <*> writeFlashCardHtml b

cards :: Parser [P.Card]
cards = many (extendedCard <|> simpleCard)

-- | Returns a list of tuples which contain rendered HTML for the front and
-- | back of the cards provided in the order that they appeared
produceDeck :: T.Text -> IO [(T.Text, T.Text)]
produceDeck input = do
  (Pandoc meta blocks) <- textToAst input
  let deck = concat $ parseAll cards blocks
  let docDeck = documenttizeDeck (Pandoc meta) deck
  renderDeck docDeck
