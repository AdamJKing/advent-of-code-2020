{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module DayTen where

import Control.Arrow (Arrow ((&&&)))
import Data.Attoparsec.ByteString.Char8 (char, decimal)
import Data.Attoparsec.ByteString.Lazy (Result (Done), parse, sepBy)
import qualified Data.ByteString.Lazy as LB
import Data.Graph.Inductive (Graph (match, mkGraph), Node)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Ix (Ix (inRange))
import Data.MemoTrie (memoFix)
import Data.Monoid (Sum (..))

dayTenInput :: IO [Int]
dayTenInput = do
  contents <- LB.readFile "data/day_ten_input.txt"
  let Done "" input = parse ((decimal `sepBy` char '\n') <* char '\n') contents
  return input

sample :: [Int]
sample = [28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3]

steps :: Num a => [a] -> [a]
steps = steps' . (0 :)
  where
    steps' (x : y : xs) = abs (x - y) : steps' (y : xs)
    steps' [_] = [3]
    steps' [] = []

countWhen :: (Int -> Bool) -> Int -> Sum Int
countWhen p a = if p a then pure 1 else mempty

countJolts :: [Int] -> (Sum Int, Sum Int)
countJolts = foldMap (countWhen (== 3) &&& countWhen (== 1))

possibleAdapterConfigs :: [Int] -> Int
possibleAdapterConfigs adapters =
  let !configs = mkGraph nodes edges :: Gr () ()
   in noPaths 0 mx configs
  where
    mx = 3 + maximum adapters
    adapters' = 0 : mx : adapters

    nodes = asNode <$> adapters'
    edges = foldMap (\x -> asEdge x <$> filter (canLink x) adapters') adapters'

    asNode n = (n, ())
    asEdge a b = (a, b, ())

    canLink from to = inRange (1, 3) (to - from)

noPaths :: Graph gr => Node -> Node -> gr () () -> Int
noPaths start end graph = memoFix (go graph) start
  where
    go gr recur start' =
      case match start' gr of
        (Just (_, n, _, children), _) ->
          if n == end
            then 1
            else sum $ recur . snd <$> children
        (Nothing, _) -> 0
