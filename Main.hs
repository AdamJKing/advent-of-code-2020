module Main where

import Data.Attoparsec.ByteString.Lazy (eitherResult, parse)
import Data.Maybe (fromJust)
import Data.Sequence as Seq (sort)
import DayFour (dayFourInput, passportList, validPassport)
import DayOne
  ( dayOneInput,
    find2NumbersAddingTo,
    find3NumbersAddingTo,
  )
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
