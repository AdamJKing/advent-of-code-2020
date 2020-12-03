module DayTwo where

import Data.Attoparsec.ByteString.Char8 (anyChar, char, decimal)
import Data.Attoparsec.ByteString.Lazy (Parser, eitherResult, parse, string, takeLazyByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LBC
import Data.Int (Int64)
import Data.String (IsString (fromString))

dayTwoInput :: IO [LB.ByteString]
dayTwoInput = do
  contents <- LB.readFile "data/day_two_input.txt"
  return (LBC.lines contents)

type RuleSet = ((Int64, Int64), Char)

type Password = LB.ByteString

ruleSet :: Parser RuleSet
ruleSet = do
  num <- (,) <$> (decimal <* char '-') <*> decimal <* char ' '
  req <- anyChar
  return (num, req)

testPassword :: RuleSet -> Password -> Bool
testPassword ((mn, mx), req) pass =
  let count = LBC.count req pass
   in (mn <= count && count <= mx)

testPassword' :: RuleSet -> Password -> Bool
testPassword' ((x, y), req) pass = 
  let inX = req == LBC.index pass (x - 1) 
      inY = req == LBC.index pass (y - 1)
  in inX /= inY

parseLine :: LB.ByteString -> Either String (RuleSet, Password)
parseLine =
  eitherResult
    . parse
      ( do
          rules <- ruleSet <* string (fromString ": ")
          password <- takeLazyByteString
          return (rules, password)
      )
