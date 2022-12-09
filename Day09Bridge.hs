{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (liftM)
import Data.IntMap.Strict ((!))
import qualified Data.IntMap.Strict as I
import Data.List (foldl')
import Data.List.Split (endBy)
import qualified Data.Set as S
import Data.Tuple (swap)
import Formatting

type Point = (Int, Int)

data Knots = Knots
  { segments :: I.IntMap Point,
    tailHistory :: [Point]
  }
  deriving (Show)

baseState :: Int -> Knots
baseState len =
  Knots
    { segments = I.fromList $ zip [1 .. len] (repeat (0, 0)),
      tailHistory = [(0, 0)]
    }

main :: IO ()
main = do
  input <- liftM (endBy "\n") . readFile $ "input/day09.txt"
  let inputMoves = concat . map parseMove $ input
      Knots {tailHistory = length2History} = foldl' (flip moveReal) (baseState 2) inputMoves -- Expected: 6332
      Knots {tailHistory = length10History} = foldl' (flip moveReal) (baseState 10) inputMoves -- Expected: 2511
  fprintLn ("Part 1 result (unique tail positions): " % int) (S.size . S.fromList $ length2History)
  fprintLn ("Part 2 result (10 knots): " % shown) (S.size $ S.fromList length10History)

-- Point transformation
infixl 6 <+>

(<+>) :: Point -> (Int, Int) -> Point
(x, y) <+> (dx, dy) = (x + dx, y + dy)

-- breaks up move into 1-steps
parseMove :: String -> [(Int, Int)]
parseMove (direction : _ : amount) = replicate amountInt increment
  where
    amountInt = read amount
    increment = case direction of
      'U' -> (0, 1)
      'D' -> (0, -1)
      'L' -> (-1, 0)
      'R' -> (1, 0)

{-
move :: Knots -> (Int, Int) -> Knots
move knots@Knots{headPos, tailPos, tailHistory} move
  -- Head/tail are already close enough
  | abs dx < 2 && abs dy < 2 || newHeadPos == tailPos = newHead
  -- too far vertically
  | dx == 0 || dy == 0 = newHead { tailPos = normalTailPos, tailHistory = normalTailPos : tailHistory }
  -- weird diagonal edge case :)
  | otherwise = newHead { tailPos = headPos, tailHistory = headPos : tailHistory }
  where
    newHeadPos = headPos <+> move
    newHead = knots { headPos = newHeadPos }
    (dx, dy) = distances tailPos newHeadPos
    normalTailPos = tailPos <+> if abs dx >= 2 then (signum dx, 0) else (0, signum dy)
    -- updatedHistory = S.insert newTailPos tailHistory
    -- updatedHistory = newTailPos : tailHistory
-}

-- updateKnots :: Knots -> (Int, Int) -> Knots
-- updateKnots kntos@Knots{segments, tailHistory}

-- newChildPos :: Point -> Point -> Point
-- newChildPos parent@(px, py) child@(cx, cy)
{-
  -- Head/tail are already close enough
  | abs dx < 2 && abs dy < 2 || newHeadPos == tailPos = newHead
  -- too far vertically
  | dx == 0 || dy == 0 = newHead { tailPos = normalTailPos, tailHistory = normalTailPos : tailHistory }
  -- weird diagonal edge case :)
  | otherwise = newHead { tailPos = headPos, tailHistory = headPos : tailHistory }
  where
    newHeadPos = headPos <+> move
    newHead = knots { headPos = newHeadPos }
    (dx, dy) = distances tailPos newHeadPos
    normalTailPos = tailPos <+> if abs dx >= 2 then (signum dx, 0) else (0, signum dy)
    -- updatedHistory = S.insert newTailPos tailHistory
    -- updatedHistory = newTailPos : tailHistory-}

{-moveFull :: Knots -> (Int, Int) -> Knots
moveFull knots move = fst $ foldl' (\(kts, mov) idx -> if mov == (0,0) then (kts, mov) else performMove kts idx mov) (knots, move) [1..9]-}

moveReal :: (Int, Int) -> Knots -> Knots
moveReal move old@Knots {segments, tailHistory} = old {segments = newSegments, tailHistory = newTail : tailHistory}
  where
    newSegments = snd $ I.mapAccumWithKey actualMove ((0, 0), move) segments
    (_, newTail) = I.findMax newSegments

actualMove :: ((Int, Int), (Int, Int)) -> Int -> Point -> (((Int, Int), (Int, Int)), Point)
actualMove (prevOld, prevNew) segNum current
  -- first segment is built different
  | segNum == 1 = ((current, current <+> prevNew), current <+> prevNew)
  -- noop (previous didn't move/already close enough)
  | prevOld == prevNew || (abs dx < 2 && abs dy < 2) = ((current, current), current)
  -- only moved vertically
  | dx == 0 && abs dy >= 2 = ((current, current <+> (0, signum dy)), current <+> (0, signum dy))
  -- only moved horizontally
  | dy == 0 && abs dx >= 2 = ((current, current <+> (signum dx, 0)), current <+> (signum dx, 0))
  -- diagonal case (?)
  | otherwise = ((current, current <+> (signum dx, signum dy)), current <+> (signum dx, signum dy))
  where
    (dx, dy) = prevNew <+> negateTuple current

-- each move is performed twice after the first :(
-- maybe move current then give distance to next iter?
{-performMove :: Knots -> Int -> (Int, Int) -> (Knots, (Int, Int))
performMove knots@Knots{segments, len, tailHistory} segNum move
  | move == (0,0) = (knots, move)
  -- Head/tail are already close enough
  | abs dx < 2 && abs dy < 2 = (newCurrent, (0, 0))
  -- end of the line!
  | segNum == len - 1 = (nextNormal { tailHistory = nextNormalPos : tailHistory }, (0, 0))
  -- effective move is wherever the tail moved to
  -- | otherwise = (nextNormal, distances next nextNormalPos)
  | otherwise = (nextNormal, nextNormalPos <+> negateTuple next)
  where
    current = segments ! segNum
    newCurrentPos = current <+> move
    newCurrent@Knots{segments=curSegs} = knots { segments = I.insert segNum newCurrentPos segments }
    next = segments ! (segNum + 1)
    -- too far vertically or horizontally (not both)
    nextNormalPos = if dx == 0 || dy == 0 then
      next <+> if abs dx >= 2 then (signum dx, 0) else (0, signum dy)
    -- weird diagonal edge case :)
    else current
    nextNormal = newCurrent { segments = I.insert (segNum + 1) nextNormalPos curSegs }
    (dx, dy) = distances next newCurrentPos
-}
-- floored distance between two points
distances :: Point -> Point -> (Int, Int)
distances (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

-- distance (x1, y1) (x2, y2) = floor . sqrt $ (fromIntegral y2 - fromIntegral y1)^2 + (fromIntegral x2 - fromIntegral x1)^2

negateTuple :: (Int, Int) -> (Int, Int)
negateTuple (x, y) = (-x, -y)
