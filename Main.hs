module Main where

import Data.Function (fix)
import Data.Maybe (fromJust)
import Data.Sequence as Seq (sort)
import DayOne
  ( dayOneInput,
    find2NumbersAddingTo,
    find3NumbersAddingTo,
  )
import DayThree (Cell (..), dayThreeInput, index)
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
  let numTrees = flip fix (0, (0, 0)) $ \loop (n, (x, y)) ->
        case index inputDayThree (x, y) of
          Just Space -> loop (n, (x + 3, y + 1))
          Just Tree -> loop (n + 1, (x + 3, y + 1))
          Nothing -> n

  print ("Part One: " ++ show numTrees)
