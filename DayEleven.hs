{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module DayEleven where

import Control.Monad (forM_)
import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.ByteString.Char8 as B
import Data.Foldable (asum)
import Data.Function (fix)
import Data.Ix (Ix (inRange))
import Data.Map.Strict (Map, elems, findMax, mapEitherWithKey, (!?))
import qualified Data.Map.Strict as Map
import Data.Semigroup (Sum)

dayElevenInput :: IO CellMap
dayElevenInput = fromByteString <$> B.readFile "data/day_eleven_input.txt"

data CellState = Taken | Empty
  deriving (Eq, Ord)

instance Show CellState where
  showsPrec _ Taken = ('#' :)
  showsPrec _ Empty = ('L' :)

type CellMap = Map (Int, Int) CellState

fromByteString :: B.ByteString -> CellMap
fromByteString bs =
  Map.fromList (go (w, h) bs [])
  where
    (Just w) = B.elemIndex '\n' bs
    h = B.count '\n' bs

    go = fix $ \loop (x, y) input cells ->
      case B.unsnoc input of
        Nothing -> cells
        Just (next, '\n') -> loop (w - 1, y - 1) next cells
        Just (next, 'L') ->
          ((x, y), Empty) : loop (x - 1, y) next cells
        Just (next, _) -> loop (x - 1, y) next cells

printMap :: CellMap -> IO ()
printMap cells =
  let (w, h) = fst $ Map.findMax cells
   in forM_ [0 .. h] $ \y ->
        print $ mconcat (map (maybe "." show . (cells !?) . (,y)) [0 .. w])

update :: CellMap -> Maybe CellMap
update cells =
  let update' o cell
        | Empty <- cell, (taken == 0) = Right Taken
        | Taken <- cell, (taken >= 5) = Right Empty
        | otherwise = Left cell
        where
          taken = sumTaken cells o

      (!old, !new) = mapEitherWithKey update' cells
   in if null new
        then Nothing
        else Just (old <> new)

inMap :: CellMap -> (Int, Int) -> Bool
inMap cells = let (limits, _) = findMax cells in inRange ((0, 0), limits)

sumTaken :: CellMap -> (Int, Int) -> Sum Int
sumTaken cells o = foldMap lookForTaken [(-1, 0), (-1, -1), (0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1)]
  where
    lookForTaken delta = if Taken `elem` castRay cells delta o then 1 else 0

castRay :: CellMap -> (Int, Int) -> (Int, Int) -> Maybe CellState
castRay cells (dx, dy) (x, y) =
  asum $
    map (cells !?) $
      takeWhile (inMap cells) $
        iterate (bimap (+ dx) (+ dy)) (x + dx, y + dy)

updateUntilStable :: CellMap -> CellMap
updateUntilStable = fix $ \loop iter -> maybe iter loop (update iter)

countOccupiedSeats :: CellMap -> Int
countOccupiedSeats = length . filter (== Taken) . elems

sample :: B.ByteString
sample =
  "L.LL.LL.LL\n\
  \LLLLLLL.LL\n\
  \L.L.L..L..\n\
  \LLLL.LL.LL\n\
  \L.LL.LL.LL\n\
  \L.LLLLL.LL\n\
  \..L.L.....\n\
  \LLLLLLLLLL\n\
  \L.LLLLLL.L\n\
  \L.LLLLL.LL\n"
