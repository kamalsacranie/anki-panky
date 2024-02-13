{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module CardParser.Utils where

import Control.Applicative
import Data.Text qualified as T
import Text.Pandoc
import Types.Parser

item :: Parser Block
item =
  P
    ( \case
        [] -> []
        (x : xs) -> [(x, xs)]
    )

satisfyBlock :: (Block -> Bool) -> Parser Block
satisfyBlock f = do
  x <- item
  if f x
    then return x
    else empty

cardBack :: Parser (Back, CardTags)
cardBack = do
  allBlocks <- reverse <$> many nonCardDelim
  tags <- case allBlocks of
    (lastBlock : _) -> case lastBlock of
      Para [RawInline (Format "html") rawHTML] -> case words $ T.unpack rawHTML of
        ("<AnkiTags" : rawTags) -> case reverse $ unwords rawTags of
          ('>' : '/' : tagString) -> return $ reverse tagString
          ('>' : tagString) -> return $ reverse tagString
          _nonTag -> return ""
        _nonTag -> return ""
      _nonTag -> return ""
    _noTag -> return ""
  return (reverse allBlocks, CTags $ words tags)

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
