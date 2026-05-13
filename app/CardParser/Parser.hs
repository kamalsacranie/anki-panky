module CardParser.Parser (parseCards) where

import CardParser.ExtCard (extendedCard)
import CardParser.SimpleCard (simpleCard)
import Control.Applicative (Alternative (many), (<|>))
import Text.Pandoc (Block, Block(HorizontalRule))
import Types.Parser

parseAll :: Parser a -> [Block] -> [a]
parseAll p bs = [a | (a, rest) <- parse p bs, all (== HorizontalRule) rest]

cards :: Parser [Card]
cards = many (extendedCard <|> simpleCard)

parseCards :: [Block] -> [Card]
parseCards blocks = concat $ parseAll cards blocks
