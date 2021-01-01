{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module DayTwelve where

import Control.Applicative (Alternative ((<|>)))
import Data.Attoparsec.ByteString.Char8 (char, decimal)
import Data.Attoparsec.ByteString.Lazy (Parser, maybeResult, parse, sepBy)
import Data.Bifunctor (Bifunctor (first, second))
import qualified Data.ByteString.Lazy as LB
import Data.Functor (($>))
import Data.Maybe (fromMaybe)

dayTwelveInput :: IO [(Movement, Integer)]
dayTwelveInput = parse' <$> LB.readFile "data/day_twelve_input.txt"
  where
    parse' = fromMaybe (error "Issues with Day Twelve input") . maybeResult . parse instructions

data Direction = North | East | South | West
  deriving (Enum, Show)

data Movement = Static Direction | Left' | Right' | Forward
  deriving (Show)

movement :: Parser Movement
movement =
  (char 'N' $> Static North)
    <|> (char 'S' $> Static South)
    <|> (char 'E' $> Static East)
    <|> (char 'W' $> Static West)
    <|> (char 'L' $> Left')
    <|> (char 'R' $> Right')
    <|> (char 'F' $> Forward)

instruction :: Parser (Movement, Integer)
instruction = (,) <$> movement <*> decimal

instructions :: Parser [(Movement, Integer)]
instructions = instruction `sepBy` char '\n'

data Map = Map {ship :: (Integer, Integer), waypoint :: (Integer, Integer)}
  deriving (Show)

moveShip :: Integer -> Map -> Map
moveShip n Map {ship, waypoint} =
  let newPosition = ship `add` (waypoint `times` n)
   in Map {ship = newPosition, waypoint}

moveWaypoint :: (Direction, Integer) -> Map -> Map
moveWaypoint (North, i) m@Map {waypoint} = m {waypoint = second (+ i) waypoint}
moveWaypoint (South, i) m@Map {waypoint} = m {waypoint = second (subtract i) waypoint}
moveWaypoint (East, i) m@Map {waypoint} = m {waypoint = first (+ i) waypoint}
moveWaypoint (West, i) m@Map {waypoint} = m {waypoint = first (subtract i) waypoint}

rotateLeft :: (Eq a, Num a) => a -> Map -> Map
rotateLeft i m@Map {waypoint} = m {waypoint = rotateLeft' i (0, 0) waypoint}
  where
    rotateLeft' 90 (x, y) (a, b) = (- (b - y) + x, (a - x) + y)
    rotateLeft' 180 (x, y) (a, b) = (- (a - x) + x, - (b - y) + y)
    rotateLeft' 270 (x, y) (a, b) = (b - y + x, - (a - x) + y)
    rotateLeft' _ _ _ = error "unsupported rotation"

rotateRight :: (Eq a, Num a) => a -> Map -> Map
rotateRight 90 = rotateLeft 270
rotateRight 180 = rotateLeft 180
rotateRight 270 = rotateLeft 90
rotateRight _ = error "unsupported rotation"

add :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
(ax, ay) `add` (bx, by) = (ax + bx, ay + by)

minus :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
(ax, ay) `minus` (bx, by) = (ax - bx, ay - by)

times :: (Integer, Integer) -> Integer -> (Integer, Integer)
(ax, ay) `times` n = (ax * n, ay * n)

starting :: Map
starting = Map {ship = (0, 0), waypoint = (10, 1)}

runInstructions :: [(Movement, Integer)] -> (Integer, Integer)
runInstructions = ship . foldl (flip run) starting
  where
    run :: (Movement, Integer) -> Map -> Map
    run (Static dir, i) = moveWaypoint (dir, i)
    run (Left', i) = rotateLeft i
    run (Right', i) = rotateRight i
    run (Forward, n) = moveShip n

sample :: LB.ByteString
sample =
  "F10\n\
  \N3\n\
  \F7\n\
  \R90\n\
  \F11"
