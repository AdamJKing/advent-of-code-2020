{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module DaySeven (daySevenInput, rules, countContaining) where

import Data.Attoparsec.ByteString.Char8 (char, digit, letter_ascii)
import Data.Attoparsec.ByteString.Lazy (Parser, many1, sepBy)
import qualified Data.ByteString.Lazy as LB
import Data.Functor (($>))
import Data.Graph (transposeG, Graph, Vertex, graphFromEdges, reachable)

daySevenInput :: IO LB.ByteString
daySevenInput = LB.readFile "data/day_seven_input.txt"

type BagKey = String

data BagLookup = BL
  { graph :: Graph,
    _nodeFromVertex :: Vertex -> ((), BagKey, [BagKey]),
    vertexFromKey :: BagKey -> Maybe Vertex
  }

rule :: Parser (BagKey, [BagKey])
rule = do
  key <- bagKey
  _ <- " contain "
  values <- contains <> ("no other bags." $> [])
  return (key, values)

bagKey :: Parser BagKey
bagKey = do
  a <- word <* char ' '
  b <- word
  _ <- " bags" <> " bag"
  return (a <> b)

contains :: Parser [BagKey]
contains = do
  canContain' <- ((digit <* char ' ') *> bagKey) `sepBy` ", "
  _ <- char '.'
  return canContain'

word :: Parser String
word = many1 letter_ascii

rules :: Parser BagLookup
rules = do
  r <- rule `sepBy` char '\n'
  let (g, nfv, vfk) = graphFromEdges (map (\(a, b) -> ((), a, b)) r)
  return (BL g nfv vfk)

countContaining :: BagLookup -> BagKey -> Int
countContaining bags key = case vertexFromKey bags key of
  Nothing -> 0
  Just k -> length (reachable (transposeG $ graph bags) k) - 1
