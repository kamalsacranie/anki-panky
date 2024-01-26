{-# LANGUAGE LambdaCase #-}

module CardParser.SimpleCard where

import CardParser.Parser
import CardParser.Utils
import Text.Pandoc
import Types

cardSimpleFront :: Parser Front
cardSimpleFront = do
  h1 <- satisfyBlock (\case (Header 1 _ _) -> True; _ -> False)
  return (SimpleFront h1)

simpleCard :: Parser Card
simpleCard = cardSimpleFront >>= \front -> Card front <$> cardBack
