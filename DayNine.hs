{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module DayNine where

import Data.Attoparsec.ByteString.Char8 (char, decimal, sepBy)
import Data.Attoparsec.ByteString.Lazy (Result (Done), parse)
import qualified Data.ByteString.Lazy as LB
import Data.Function (fix, (&))
import Data.Sequence (Seq ((:<|), (:|>)))
import qualified Data.Sequence as Seq
import DayOne (find2NumbersAddingTo)
import Data.Semigroup (Max(Max), Min(Min))
import Control.Arrow (Arrow((&&&)))

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

findContiguousSeqAddingTo :: Int -> Seq Int -> Maybe Int
findContiguousSeqAddingTo n input = do
  (Min mn, Max mx) <- foldMap (Min &&& Max) <$> go input
  return (mn + mx)
  where
    go = fix $ \loop -> \case
      Seq.Empty -> Nothing
      xs -> case sumsTo n (Seq.Empty, xs) of
        Nothing -> loop (Seq.drop 1 xs)
        Just a -> Just a

sumsTo :: Int -> (Seq Int, Seq Int) -> Maybe (Seq Int)
sumsTo n =
  0
    & fix
      ( \loop acc ->
          \case
            (xs, Seq.Empty) -> if acc == n then Just xs else Nothing
            (xs, y :<| ys) -> case compare (acc + y) n of
              EQ -> Just (xs :|> y)
              GT -> Nothing
              LT -> loop (acc + y) (xs :|> y, ys)
      )
