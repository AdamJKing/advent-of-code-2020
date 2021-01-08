{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module DayThirteen where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (forM, guard)
import Data.Attoparsec.ByteString.Char8 (char, decimal)
import Data.Attoparsec.ByteString.Lazy (Parser, maybeResult, parse, sepBy1)
import qualified Data.ByteString.Lazy as LB
import Data.Foldable (find)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid (First (..))
import Data.Semigroup (All (All, getAll))
import Data.Traversable (for)

data Notes = Notes {earliest :: Int, buses :: NonEmpty (Maybe Int)}
  deriving (Show)

dayThirteenInput :: IO Notes
dayThirteenInput = parse' <$> LB.readFile "data/day_thirteen_input.txt"
  where
    parse' = fromMaybe (error "Issues with Day Thirteen input") . maybeResult . parse note

note :: Parser Notes
note = Notes <$> (decimal <* char '\n') <*> listOfOnly decimal
  where
    listOfOnly p = NE.fromList <$> (Just <$> p <|> (char 'x' $> Nothing)) `sepBy1` char ','

waitTime :: Notes -> Maybe Int
waitTime Notes {earliest, buses} = do
  (timestamp, bus) <- getFirst $ foldMap (First . usable) [earliest ..]
  return (bus * (timestamp - earliest))
  where
    activeBuses = catMaybes (NE.toList buses)
    usable i = (i,) <$> find ((== 0) . (i `mod`)) activeBuses

sample :: LB.ByteString
sample =
  "939\n\
  \7,13,x,x,59,x,31,19"

-- 0 = ka - (kb + 1) - (kc + 4) - (kd + 6)
-- 0 = 7k - (13k + 1) - (59k + 4) - (31k + 6) - (19k + 7)

-- 0 = 0 - 1 + 4 - 6 + 7
