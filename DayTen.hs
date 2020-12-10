{-# LANGUAGE OverloadedStrings #-}

module DayTen where

import Control.Arrow (Arrow ((&&&)))
import Data.Attoparsec.ByteString.Char8 (char, decimal)
import Data.Attoparsec.ByteString.Lazy (Result (Done), parse, sepBy)
import qualified Data.ByteString.Lazy as LB
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
