{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module DayEleven where

import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as B
import Data.Function (fix)
import Data.Ix (Ix (inRange))
import Data.Map (Map, elems, mapEither, (!), (!?))
import qualified Data.Map.Strict as Map
import Debug.Trace (traceShowId)

dayElevenInput :: IO CellMap
dayElevenInput = fromByteString <$> B.readFile "data/day_eleven_input.txt"

data CellState = Taken | Empty
  deriving (Eq, Ord)

data Cell = Cell {state :: !CellState, neighbours :: ![(Int, Int)]}

instance Show Cell where
  showsPrec _ (Cell Taken _) = ('#' :)
  showsPrec _ (Cell Empty _) = ('L' :)

type CellMap = Map (Int, Int) Cell

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
          let neighbours =
                [ (nx, ny)
                  | nx <- [(x - 1) .. (x + 1)],
                    ny <- [(y - 1) .. (y + 1)],
                    (x, y) /= (nx, ny),
                    lookupXY nx ny == Just 'L'
                ]
           in ((x, y), Cell Empty neighbours) : loop (x - 1, y) next cells
        Just (next, _) -> loop (x - 1, y) next cells

    lookupXY x y
        | inRange ((0, 0), (w - 1, h - 1)) (x, y) =
            Just $ B.index bs $ max 0 ((w + 1) * y) + x
        | otherwise  = Nothing

printMap :: CellMap -> IO ()
printMap cells =
  let (w, h) = fst $ Map.findMax cells
  in
    forM_ [0 .. h] $ \y ->
      print $ mconcat (map (maybe "." show . (cells !?) . (,y)) [0 .. w])

update :: CellMap -> Maybe CellMap
update cells =
  let update' Cell {..}
        | Empty <- state, (count Taken neighbours == 0) = Right (Cell Taken neighbours)
        | Taken <- state, (count Taken neighbours >= 4) = Right (Cell Empty neighbours)
        | otherwise = Left (Cell state neighbours)

      (old, new) = mapEither update' cells
   in if null new
        then Nothing
        else Just (old <> new)
  where
    count s = length . filter (== s) . map (state . (cells !))

updateUntilStable :: CellMap -> CellMap
updateUntilStable = fix $ \loop iter -> maybe iter loop (update iter)

countOccupiedSeats :: CellMap -> Int
countOccupiedSeats = length . filter ((== Taken) . state) . elems

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
