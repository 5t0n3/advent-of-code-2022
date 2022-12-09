{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (liftM)
import Data.IntMap.Strict ((!))
import qualified Data.IntMap.Strict as I
import Data.List (foldl')
import Data.List.Split (endBy)
import qualified Data.Set as S
import Formatting

type Point = (Int, Int)

data Rope = Rope
  { knots :: I.IntMap Point,
    tailHistory :: S.Set Point
  }

startingRope :: Int -> Rope
startingRope len =
  Rope
    { knots = I.fromList $ zip [1 .. len] (repeat (0, 0)),
      tailHistory = S.empty
    }

main :: IO ()
main = do
  input <- liftM (endBy "\n") . readFile $ "input/day09.txt"
  let inputMoves = concat . map parseMove $ input
      length2Total = countTailPositions 2 inputMoves -- Expected: 6332
      length10Total = countTailPositions 10 inputMoves -- Expected: 2511
  fprintLn ("Part 1 result (2 knots): " % int) length2Total
  fprintLn ("Part 2 result (10 knots): " % int) length10Total
  where
    countTailPositions len = S.size . tailHistory . foldl' moveRope (startingRope len)

-- Point transformation
infixl 6 <+>

(<+>) :: Point -> (Int, Int) -> Point
(x, y) <+> (dx, dy) = (x + dx, y + dy)

-- Moves are broken up into individual steps since the tail has to be updated after every one
parseMove :: String -> [(Int, Int)]
parseMove (direction : _ : amount) = replicate (read amount) increment
  where
    increment = case direction of
      'U' -> (0, 1)
      'D' -> (0, -1)
      'L' -> (-1, 0)
      'R' -> (1, 0)

-- Updates the positions of every knot along the rope after performing the given move
moveRope :: Rope -> (Int, Int) -> Rope
moveRope Rope {knots, tailHistory} move = Rope {knots = newKnots, tailHistory = S.insert newTail tailHistory}
  where
    newKnots = snd $ I.mapAccumWithKey actualMove move knots
    -- findMax returns the value associated with the maximum key in a map
    -- in this case, it always returns the position of the tail
    (_, newTail) = I.findMax newKnots

actualMove :: Point -> Int -> Point -> (Point, Point)
actualMove prevNew knotNum current
  -- for the first knot, prevNew is just the input move
  | knotNum == 1 = dupe $ current <+> prevNew
  -- already close enough to the previous knot
  | (abs dx < 2 && abs dy < 2) = dupe current
  -- need to move vertically (no motion horizontally)
  | dx == 0 = dupe $ current <+> (0, signum dy)
  -- need to move horizontally (no motion vertically)
  | dy == 0 = dupe $ current <+> (signum dx, 0)
  -- need to move diagonally (always 1 unit vertically/horizontally towards previous knot)
  | otherwise = dupe $ current <+> (signum dx, signum dy)
  where
    (dx, dy) = prevNew <+> negateTuple current

negateTuple :: (Int, Int) -> (Int, Int)
negateTuple (x, y) = (-x, -y)

dupe :: a -> (a, a)
dupe val = (val, val)
