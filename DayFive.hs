module DayFive where

import Control.Arrow (Arrow ((***)))
import qualified Data.ByteString.Lazy.Char8 as LB

dayFiveInput :: IO [LB.ByteString]
dayFiveInput = LB.lines <$> LB.readFile "data/day_five_input.txt"

determineRow :: LB.ByteString -> Int
determineRow = snd . LB.foldl' updateRow (0, 127)
  where
    updateRow (l, h) 'F' = (l, (l + h) `quot` 2)
    updateRow (l, h) 'B' = (l + (((h + 1) - l) `quot` 2), h)
    updateRow r _ = r

determineColumn :: LB.ByteString -> Int
determineColumn = snd . LB.foldl' updateRow (0, 7)
  where
    updateRow (l, h) 'L' = (l, (l + h) `quot` 2)
    updateRow (l, h) 'R' = (l + (((h + 1) - l) `quot` 2), h)
    updateRow r _ = r

determineSeatId :: LB.ByteString -> Int
determineSeatId = calculateId . (determineRow *** determineColumn) . LB.splitAt 7
  where
    calculateId (r, c) = r * 8 + c


