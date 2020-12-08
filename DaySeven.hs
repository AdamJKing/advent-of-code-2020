{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module DaySeven (daySevenInput, rules, countContaining, countNestedBags) where

import Data.Attoparsec.ByteString.Char8 (char, decimal, letter_ascii)
import Data.Attoparsec.ByteString.Lazy (Parser, many1, sepBy)
import qualified Data.ByteString.Lazy as LB
import Data.Functor (($>))
import Data.Graph.Inductive
  ( Gr,
    Graph (match, mkGraph),
    LEdge,
    UNode,
    grev,
    lsuc',
    reachable,
  )
import Data.Hashable (Hashable (hash))

daySevenInput :: IO LB.ByteString
daySevenInput = LB.readFile "data/day_seven_input.txt"

type BagKey = String

type BagLookup = Gr () Int

rule :: Parser (UNode, [LEdge Int])
rule = do
  key <- bagKey
  _ <- " contain "
  values <- contains key <> ("no other bags." $> [])
  return ((hash key, ()), values)

bagKey :: Parser BagKey
bagKey = do
  a <- word <* char ' '
  b <- word
  _ <- " bags" <> " bag"
  return (a <> b)

contains :: BagKey -> Parser [LEdge Int]
contains key = (edge `sepBy` ", ") <* char '.'
  where
    edge = do
      v <- decimal <* char ' '
      k <- bagKey
      return (hash key, hash k, v)

word :: Parser String
word = many1 letter_ascii

rules :: Parser BagLookup
rules = do
  rs <- rule `sepBy` char '\n'
  let (nodes, mconcat -> edges) = unzip rs

  return $ mkGraph nodes edges

countContaining :: BagLookup -> BagKey -> Int
countContaining bags key = length (reachable (hash key) (grev bags)) - 1

countNestedBags :: BagKey -> BagLookup -> Int
countNestedBags key = subtract 1 . countNestedBags' (hash key)
  where
    countNestedBags' node graph =
      case match node graph of
        (Nothing, _) -> 1
        (Just ctx, graph') -> 1 + sum (fmap (\(node', edge) -> edge * countNestedBags' node' graph') (lsuc' ctx))

sample :: LB.ByteString
sample =
  "shiny gold bags contain 2 dark red bags.\n\
  \dark red bags contain 2 dark orange bags.\n\
  \dark orange bags contain 2 dark yellow bags.\n\
  \dark yellow bags contain 2 dark green bags.\n\
  \dark green bags contain 2 dark blue bags.\n\
  \dark blue bags contain 2 dark violet bags.\n\
  \dark violet bags contain no other bags."
