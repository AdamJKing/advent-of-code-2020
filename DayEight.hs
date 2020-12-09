{-# LANGUAGE LambdaCase #-}
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
import Data.Function (fix, (&))
import Data.Maybe (fromMaybe)
import Data.Monoid (First (..))
import qualified Data.Sequence as Seq

data Instruction = NOP Int | ACC Int | JMP Int
  deriving (Show)

type Instructions = Seq.Seq Instruction

instructions :: Parser Instructions
instructions = Seq.fromList <$> instruction `sepBy` "\n"
  where
    instruction = choice [nop, acc, jmp]

    nop = NOP <$> (string "nop " *> signed decimal)
    acc = ACC <$> (string "acc " *> signed decimal)
    jmp = JMP <$> (string "jmp " *> signed decimal)

dayEightInput :: IO Instructions
dayEightInput = parse' <$> LB.readFile "data/day_eight_input.txt"
  where
    parse' = fromMaybe (error "Issues with Day Eight input") . maybeResult . parse instructions

run :: Instructions -> Maybe Int
run instructions' = go 0 0 []
  where
    go = fix $ \loop i acc visited ->
      if
          | i == Seq.length instructions' -> Just acc
          | i `elem` visited -> Nothing
          | NOP _ <- (Seq.index instructions' i) -> loop (i + 1) acc (i : visited)
          | (ACC delta) <- (Seq.index instructions' i) -> loop (i + 1) (acc + delta) (i : visited)
          | (JMP delta) <- (Seq.index instructions' i) -> loop (i + delta) acc (i : visited)
          | otherwise -> Just acc

fixInstructions :: Seq.Seq Instruction -> Maybe Int
fixInstructions instructions' =
  let possibleInstructionSets =
        instructions'
          & Seq.findIndicesL
            ( \case
                (NOP 0) -> False
                (NOP _) -> True
                (JMP _) -> True
                _ -> False
            )
          & fmap (\i -> Seq.adjust flipInstruction i instructions')
   in getFirst $ foldMap (First . run) possibleInstructionSets
  where
    flipInstruction (NOP delta) = JMP delta
    flipInstruction (JMP delta) = NOP delta
    flipInstruction _ = error "tried to flip an unflippable instruction!"

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

-- don't flip NOP +0; always an infinite loop
-- it could be the JMP that caused the initial loop
-- or one of the NOP instructions before it