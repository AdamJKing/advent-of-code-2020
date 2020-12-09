{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module DayEight where

import Data.Attoparsec.ByteString.Char8
  ( Parser,
    choice,
    decimal,
    sepBy,
    signed,
    string,
  )
import Data.Attoparsec.ByteString.Lazy (maybeResult, parse)
import qualified Data.ByteString.Lazy as LB
import Data.Function (fix)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as Vec

data Instruction = NOP | ACC Int | JMP Int
  deriving (Show)

type Instructions = Vec.Vector Instruction

instructions :: Parser Instructions
instructions = Vec.fromList <$> instruction `sepBy` "\n"
  where
    instruction = choice [nop, acc, jmp]

    nop = NOP <$ (string "nop " *> signed decimal)
    acc = ACC <$> (string "acc " *> signed decimal)
    jmp = JMP <$> (string "jmp " *> signed decimal)

dayEightInput :: IO Instructions
dayEightInput = parse' <$> LB.readFile "data/day_eight_input.txt"
  where
    parse' = fromMaybe (error "Issues with Day Eight input") . maybeResult . parse instructions

run :: Instructions -> Int
run instructions' = go 0 0 []
  where
    go = fix $ \loop i acc visited ->
      if
          | i == Vec.length instructions' -> acc
          | i `elem` visited -> acc
          | NOP <- (instructions' Vec.! i) -> loop (i + 1) acc (i : visited)
          | (ACC delta) <- (instructions' Vec.! i) -> loop (i + 1) (acc + delta) (i : visited)
          | (JMP delta) <- (instructions' Vec.! i) -> loop (i + delta) acc (i : visited)

sample :: LB.ByteString
sample =
  "nop +0\n\
  \acc +1\n\
  \jmp +4\n\
  \acc +3\n\
  \jmp -3\n\
  \acc -99\n\
  \acc +1\n\
  \jmp -4\n\
  \acc +6"
