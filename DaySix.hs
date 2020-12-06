module DaySix (daySixInput, group, groups) where

import Control.Applicative (Alternative ((<|>)))
import Data.Attoparsec.ByteString.Char8 (Parser, char, notChar, sepBy)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Set as Set

daySixInput :: IO LB.ByteString
daySixInput = LB.readFile "data/day_six_input.txt"

group :: Parser (Set.Set Char)
group = set (maybeNewline (notChar '\n'))
  where
    maybeNewline a = (a <* char '\n') <|> a

groups :: Parser [Set.Set Char]
groups = group `sepBy` char '\n'

set :: Ord a => Parser a -> Parser (Set.Set a)
set single = do
  one <- single
  more <- set single <|> pure Set.empty
  return (Set.insert one more)
