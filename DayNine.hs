{-# LANGUAGE OverloadedStrings #-}

module DayNine where

import Data.Attoparsec.ByteString.Char8 (char, decimal, sepBy)
import Data.Attoparsec.ByteString.Lazy (Result (Done), parse)
import qualified Data.ByteString.Lazy as LB
import Data.Sequence (Seq ((:<|)))
import qualified Data.Sequence as Seq
import DayOne (find2NumbersAddingTo)

dayNineInput :: IO (Seq.Seq Int)
dayNineInput = do
  contents <- LB.readFile "data/day_nine_input.txt"
  let Done "" input = parse (decimal `sepBy` char '\n') contents
  return (Seq.fromList input)

findInvalidInt :: Int -> Seq Int -> Maybe Int
findInvalidInt n input = do
  case Seq.splitAt n input of
    (Seq.Empty, _) -> Nothing
    (_, Seq.Empty) -> Nothing
    (preN, y :<| _) ->
      case find2NumbersAddingTo y (Seq.sort preN) of
        Just _ -> findInvalidInt n (Seq.drop 1 input)
        Nothing -> Just y
