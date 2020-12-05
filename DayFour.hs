{-# LANGUAGE OverloadedStrings #-}

module DayFour where

import Control.Applicative (Alternative ((<|>)))
import Data.Attoparsec.ByteString.Char8 (char)
import Data.Attoparsec.ByteString.Lazy
  ( Parser,
    notInClass,
    sepBy,
    sepBy1,
    skipWhile,
    string,
  )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Set as Set

dayFourInput :: IO LB.ByteString
dayFourInput = LB.readFile "data/day_four_input.txt"

field :: B.ByteString -> b -> Parser b
field desc ctr = do
  _ <- string desc
  _ <- char ':'
  _ <- skipWhile (notInClass " \n")
  return ctr

data PassportField = BYR | IYR | EYR | HGT | HCL | ECL | PID | CID
  deriving (Eq, Ord, Show)

type Passport = [PassportField]

passportList :: Parser [Passport]
passportList = passport `sepBy` char '\n'

validPassport :: [PassportField] -> Bool
validPassport fields = Set.fromList [BYR, IYR, EYR, HGT, HCL, ECL, PID] `Set.isSubsetOf` Set.fromList fields

passport :: Parser Passport
passport = passportField `sepBy1` (char ' ' <|> char '\n') <* char '\n'

passportField :: Parser PassportField
passportField =
  field "byr" BYR
    <|> field "iyr" IYR
    <|> field "eyr" EYR
    <|> field "hgt" HGT
    <|> field "hcl" HCL
    <|> field "ecl" ECL
    <|> field "pid" PID
    <|> field "cid" CID
