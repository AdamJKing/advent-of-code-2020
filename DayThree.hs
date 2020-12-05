{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module DayThree (Cell (Space, Tree), Map, dayThreeInput, index, checkSlopes) where

import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Function (fix)
import Data.Vector (Vector)
import qualified Data.Vector as Vec

data Cell = Space | Tree

instance Show Cell where
  showsPrec _ Space = ('.' :)
  showsPrec _ Tree = ('#' :)

data Map = Map !Int !(Vector Cell)

-- wraps on width, but height is max
index :: Map -> (Int, Int) -> Maybe Cell
index (Map width vec) (x, y) =
  vec Vec.!? ((x `mod` width) + (y * width))

instance Show Map where
  show (Map 0 _) = error "empty map"
  show (Map width vec) = Vec.ifoldr' draw "\n" vec
    where
      draw ix c s =
        if ix > 0 && ix `mod` width == 0
          then '\n' : shows c s
          else shows c s

dayThreeInput :: IO Map
dayThreeInput = do
  let knownSize = 10044 -- 30 width * 324 length
  let knownWidth = 31
  readMap knownWidth knownSize <$> LB.readFile "data/day_three_input.txt"

checkSlopes :: Map -> (Int, Int) -> Int
checkSlopes world gradient =
  flip fix (0, (0, 0)) $ \loop (n, ix) ->
    case index world ix of
      Just Space -> loop (n, ix `add` gradient)
      Just Tree -> loop (n + 1, ix `add` gradient)
      Nothing -> n
  where
    add (x, y) (x', y') = (x + x', y + y')

readMap :: Int -> Int -> LB.ByteString -> Map
readMap width size contents = do
  Map width (Vec.unfoldrN size parse contents)
  where
    parse raw = do
      (x, xs) <- LB.uncons raw
      case x of
        '.' -> return (Space, xs)
        '#' -> return (Tree, xs)
        _ -> parse xs
