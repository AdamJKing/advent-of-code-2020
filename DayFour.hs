{-# LANGUAGE OverloadedStrings #-}

module DayFour where

import Control.Applicative (Alternative ((<|>)), many)
import Control.Monad (mfilter)
import Data.Attoparsec.ByteString.Char8
  ( char,
    decimal,
    inClass,
    isDigit,
    notInClass,
    satisfy,
  )
import Data.Attoparsec.ByteString.Lazy
  ( choice,
    count,
    manyTill,
    skipMany,
    (<?>),
  )
import Data.Attoparsec.ByteString.Lazy as PL
  ( Parser,
    sepBy,
    string,
  )
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Functor (($>))
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Data.String (fromString)

dayFourInput :: IO LB.ByteString
dayFourInput = LB.readFile "data/day_four_input.txt"

field :: String -> Parser b -> Parser b
field desc fieldParser = do
  _ <- string (fromString desc)
  _ <- char ':'
  (fieldParser <?> desc) <* (char ' ' <|> char '\n')

anyField :: Parser ()
anyField = do
  _ <- satisfy (notInClass " \n") `manyTill` char ':'
  _ <- skipMany (satisfy (notInClass " \n"))
  _ <- char ' ' <|> char '\n'
  return ()

birthYear :: Parser PassportField
birthYear = mfilter (\year -> 1920 <= year && year <= 2002) decimal $> BYR

issueYear :: Parser PassportField
issueYear =
  mfilter (\year -> 2010 <= year && year <= 2020) decimal $> IYR

expirationYear :: Parser PassportField
expirationYear = do
  mfilter (\year -> 2020 <= year && year <= 2030) decimal $> EYR

height :: Parser PassportField
height = centimetres <|> inches
  where
    centimetres = do
      _ <- mfilter (\n -> 150 <= n && n <= 193) decimal
      _ <- string "cm"
      return HGT

    inches = do
      _ <- mfilter (\n -> 59 <= n && n <= 76) decimal
      _ <- string "in"
      return HGT

hairColour :: Parser PassportField
hairColour = char '#' *> count 6 (satisfy (inClass "0123456789abcdef")) $> HCL

eyeColour :: Parser PassportField
eyeColour = choice ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"] $> ECL

passportId :: Parser PassportField
passportId = count 9 (satisfy isDigit) $> PID

data PassportField = BYR | IYR | EYR | HGT | HCL | ECL | PID | CID
  deriving (Eq, Ord, Show)

type Passport = [PassportField]

passportList :: Parser [Passport]
passportList = passport `sepBy` char '\n'

validPassport :: [PassportField] -> Bool
validPassport fields = Set.fromList [BYR, IYR, EYR, HGT, HCL, ECL, PID] `Set.isSubsetOf` Set.fromList fields

passport :: Parser Passport
passport = fields
  where
    fields = catMaybes <$> many single
    single = (Just <$> passportField) <|> (anyField $> Nothing)

passportField :: Parser PassportField
passportField =
  choice
    [ field "byr" birthYear,
      field "iyr" issueYear,
      field "eyr" expirationYear,
      field "hgt" height,
      field "hcl" hairColour,
      field "ecl" eyeColour,
      field "pid" passportId
    ]
