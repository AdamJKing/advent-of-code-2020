module Main where

import Data.Attoparsec.ByteString.Lazy (eitherResult, parse)
import Data.Foldable (find)
import qualified Data.IntSet as IntSet
import qualified Data.List as List
import Data.Maybe (fromJust)
import Data.Monoid (Sum (Sum))
import qualified Data.Sequence as Seq (sort)
import qualified Data.Set as Set
import DayEight (dayEightInput, fixInstructions)
import DayFive (dayFiveInput, determineSeatId)
import DayFour (dayFourInput, passportList, validPassport)
import DayNine (dayNineInput, findContiguousSeqAddingTo, findInvalidInt)
import DayOne
  ( dayOneInput,
    find2NumbersAddingTo,
    find3NumbersAddingTo,
  )
import DaySeven (countContaining, countNestedBags, daySevenInput, rules)
import DaySix (daySixInput, groups)
import DayTen (countJolts, dayTenInput, possibleAdapterConfigs, steps)
import DayThree (checkSlopes, dayThreeInput)
import DayTwo
  ( dayTwoInput,
    parseLine,
    testPassword,
    testPassword',
  )

sumWhen :: (Foldable t, Functor t) => (a -> Bool) -> t a -> Int
sumWhen f = sum . fmap (inc . f)
  where
    inc True = 1
    inc False = 0

main :: IO ()
main = do
  inputDayOne <- Seq.sort <$> dayOneInput
  let (a, b) = fromJust $ find2NumbersAddingTo 2020 inputDayOne
  print "Day One"
  print ("Part One: " ++ show (a * b))

  let (c, d, e) = fromJust $ find3NumbersAddingTo 2020 inputDayOne
  print ("Part Two: " ++ show (c * d * e))

  inputDayTwo <- dayTwoInput
  print "Day Two"
  case traverse parseLine inputDayTwo of
    Right passwords -> do
      print ("Part One: " ++ show (sumWhen (uncurry testPassword) passwords))
      print ("Part Two: " ++ show (sumWhen (uncurry testPassword') passwords))
    Left err -> print $ "Could not parse input for Day Two: " ++ err

  inputDayThree <- dayThreeInput
  print "Day Three"
  print ("Part One: " ++ show (checkSlopes inputDayThree (3, 1)))
  print ("Part Two: " ++ show (product $ checkSlopes inputDayThree <$> [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]))

  inputDayFour <- dayFourInput
  print "Day Four"
  print "Part One: Invalidated by changes"
  case eitherResult $ parse passportList inputDayFour of
    Right passports -> print ("Part Two:" ++ show (sumWhen validPassport passports))
    Left err -> print $ "Could not parse input for Day Four: " ++ err

  inputDayFive <- dayFiveInput
  print "Day Five"
  let seatIds = determineSeatId <$> inputDayFive
  let maxSeatId = maximum seatIds
  print ("Part One: " ++ show maxSeatId)
  let seatIds' = IntSet.fromList seatIds
  print ("Part Two: " ++ show (find (not . (`IntSet.member` seatIds')) [(minimum seatIds) .. maxSeatId]))

  inputDaySix <- daySixInput
  print "Day Six"
  case eitherResult $ parse groups inputDaySix of
    Right gs -> do
      print "Part One: Invalidated by changes"
      print ("Part Two: " ++ show (sum (Set.size <$> gs)))
    Left err -> print ("Issue with Day Six input: " ++ err)

  inputDaySeven <- daySevenInput
  print "Day Seven"
  case eitherResult $ parse rules inputDaySeven of
    Right bags -> do
      print ("Part One: " ++ show (countContaining bags "shinygold"))
      print ("Part Two: " ++ show (countNestedBags "shinygold" bags))
    Left err -> print ("Issue with Day Seven Input: " ++ err)

  inputDayEight <- dayEightInput
  print "Day Eight"
  print "Part One: Invalidated by changes"
  print ("Part Two: " ++ show (fixInstructions inputDayEight))

  inputDayNine <- dayNineInput
  print "Day Nine"
  print ("Part One: " ++ show (findInvalidInt 25 inputDayNine))
  print ("Part Two: " ++ show (findContiguousSeqAddingTo 177777905 inputDayNine))

  inputDayTen <- dayTenInput
  print "Day Ten"
  let diffs = steps (List.sort inputDayTen)
  let (Sum jolts3, Sum jolts1) = countJolts diffs
  print ("Part One: " ++ show (jolts1 * jolts3))
  print ("Part Two: " ++ show (possibleAdapterConfigs inputDayTen))
