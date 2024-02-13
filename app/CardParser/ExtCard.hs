{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module CardParser.ExtCard where

import CardParser.Utils
import Control.Applicative
import Data.Functor
import Text.Pandoc
import Types.Parser

anonFront :: Parser Block
anonFront = satisfyExactBlock HorizontalRule

cardExtFrontHeader :: Parser [Block]
cardExtFrontHeader =
  ( do
      h1 <- header 1
      return [h1]
  )
    <|> (anonFront $> [])

cardExtFrontBody :: Parser [Block]
cardExtFrontBody =
  many $
    satisfyBlock
      ( \case
          (Header 1 _ _) -> False
          Para [Str ".", Space, Str ".", Space, Str "."] -> False
          _ -> True
      )

cardExtendedFront :: Parser Front
cardExtendedFront = do
  h <- cardExtFrontHeader
  body <- cardExtFrontBody
  _ <- backCardStart
  return (ExtendedFront (h ++ body))

extendedCard :: Parser Card
extendedCard = cardExtendedFront >>= \front -> uncurry (Card front) <$> cardBack
