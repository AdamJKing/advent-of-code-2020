module Main where

import Data.Maybe (fromJust)
import Data.Sequence as Seq (sort)
import DayOne
  ( dayOneInput,
    find2NumbersAddingTo,
    find3NumbersAddingTo,
  )
import DayTwo (testPassword, dayTwoInput, parseLine)

sumWhen :: (Foldable t, Functor t) => (a -> Bool) -> t a -> Int
sumWhen f = sum . fmap (inc . f) 
  where inc True = 1
        inc False = 0

main :: IO ()
main = do
  inputDayOne <- Seq.sort <$> dayOneInput
  let (a, b) = fromJust $ find2NumbersAddingTo 2020 inputDayOne
  print "Day One"
  print ("Part One: " ++ show (a * b))

  let (x, y, z) = fromJust $ find3NumbersAddingTo 2020 inputDayOne
  print ("Part Two: " ++ show (x * y * z))

  inputDayTwo <- dayTwoInput
  print "Day Two"
  case traverse parseLine inputDayTwo of
    Right passwords ->
      print ("Part One: " ++ show (sumWhen (uncurry testPassword) passwords))
    Left err -> print $ "Could not parse input for Day Two: " ++ err
