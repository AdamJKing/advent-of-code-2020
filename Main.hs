{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StrictData #-}

module Main where

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Maybe (fromJust)
import Data.Sequence as Seq (Seq(Empty, (:<|), (:|>)), sort, unfoldr)

dayOneInput :: IO (Seq Int)
dayOneInput = do
  contents <- LB.readFile "data/day_one_input.txt"
  let f chunk = do
        (int, rest) <- LB.readInt chunk
        return (int, LB.drop 1 rest)

  return (Seq.unfoldr f contents)

find2NumbersAddingTo :: Int -> Seq Int -> Maybe (Int, Int)
find2NumbersAddingTo n (l :<| (ns :|> r)) =
  case compare (l + r) n of
    EQ -> Just (l, r)
    LT -> find2NumbersAddingTo n (ns :|> r)
    GT -> find2NumbersAddingTo n (l :<| ns)
find2NumbersAddingTo _ _ = Nothing

find3NumbersAddingTo :: Int -> Seq Int -> Maybe (Int, Int, Int)
find3NumbersAddingTo _ Seq.Empty = Nothing
find3NumbersAddingTo n (x :<| ns) =
  case find2NumbersAddingTo (n - x) ns of
    Just (y, z) -> Just (x, y, z)
    Nothing -> find3NumbersAddingTo n ns

main :: IO ()
main = do
  inputDayOne <- Seq.sort <$> dayOneInput
  let (a, b) = fromJust $ find2NumbersAddingTo 2020 inputDayOne
  print "Day One"
  print ("Part One: " ++ show (a * b))

  let (x, y, z) = fromJust $ find3NumbersAddingTo 2020 inputDayOne
  print ("Part Two: " ++ show (x * y * z))
