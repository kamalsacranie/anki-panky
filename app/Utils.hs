{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module CardParser.Utils where

import CardParser.Parser
import Control.Applicative
import Data.Text (pack)
import Text.Pandoc
import Types

satisfyBlock :: (Block -> Bool) -> Parser Block
satisfyBlock f = do
  x <- item
  if f x
    then return x
    else empty

cardBack :: Parser Back
cardBack = wrapInDiv "card-back" <$> many nonCardDelim

header :: Int -> Parser Block
header level = satisfyBlock (\case (Header l _ _) -> l == level; _ -> False)

nonCardDelim :: Parser Block
nonCardDelim = satisfyBlock (\case HorizontalRule -> False; (Header 1 _ _) -> False; _ -> True)

backCardStart :: Parser Block
backCardStart = satisfyExactBlock (Para [Str ".", Space, Str ".", Space, Str "."])

satisfyExactBlock :: Block -> Parser Block
satisfyExactBlock b = do
  x <- item
  if x == b
    then return x
    else empty
