{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import qualified Data.Ix as I
import Data.List (foldl', partition, sort)
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Data.Void (Void)
import Formatting (fprintLn, int, (%))
import Text.Megaparsec
import Text.Megaparsec.Char (eol, space)
import Text.Megaparsec.Char.Lexer (decimal, signed)

type Parser = Parsec Void String

type Point = (Int, Int)

data Sensor = Sensor
  { sensor :: Point,
    beacon :: Point
  }
  deriving (Show)

parseSensor :: Parser Sensor
parseSensor = do
  chunk "Sensor at "
  sensor <- parsePoint
  chunk ": closest beacon is at "
  beacon <- parsePoint
  eol
  return $ Sensor {sensor, beacon}

parsePoint :: Parser Point
parsePoint = (,) <$> (chunk "x=" *> signedInt) <*> (chunk ", y=" *> signedInt)
  where
    signedInt = signed space decimal

manhattanDistance :: Point -> Point -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

offLimits :: Int -> Sensor -> Maybe (Int, Int)
offLimits yCoord Sensor {sensor = sensor@(x, y), beacon}
  | distToRow > dist = Nothing
  | otherwise = Just (x - maxHorizontal, x + maxHorizontal)
  where
    dist = manhattanDistance sensor beacon
    distToRow = abs (y - yCoord)
    -- width of illegal range decreases by 1 on each side for every unit away from the sensor
    maxHorizontal = dist - distToRow

fuseRanges :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
fuseRanges rs range
  | null redundant = range : fine
  | otherwise = fused : fine
  where
    -- fusable ranges either overlap or are adjacent to each other
    fusable a@(a1, a2) b@(b1, b2) = I.inRange b a1 || I.inRange a b1 || a1 == b2 + 1 || b1 == a2 + 1
    (redundant, fine) = partition (fusable range) rs
    (starts, ends) = unzip (range : redundant)
    fused = (minimum starts, maximum ends)

offLimitsRanges :: [Sensor] -> Int -> [(Int, Int)]
offLimitsRanges sensors rowY = foldl' fuseRanges [] ranges
  where
    ranges = mapMaybe (offLimits rowY) sensors

offLimitsInRow :: [Sensor] -> Int -> Int
offLimitsInRow sensors rowY = overestimate - beaconOverlap
  where
    ranges = offLimitsRanges sensors rowY
    overestimate = sum $ map I.rangeSize ranges
    beaconOverlap = beaconsInRanges ranges rowY (map beacon sensors)

beaconsInRanges :: [(Int, Int)] -> Int -> [Point] -> Int
beaconsInRanges ranges rowY beacons = S.size overlapSet
  where
    inRowRange (x, y) = y == rowY && any (flip I.inRange x) ranges
    overlapSet = S.fromList $ filter inRowRange beacons

findBeacon :: [Sensor] -> Point
findBeacon sensors = go [0 .. 4000000]
  where
    truncateRanges = map (\(start, end) -> (max start 0, min end 4000000))
    go [] = error "No beacon found"
    go (y : ys)
      -- if there's two ranges, that means they couldn't be fused & are therefore disjoint
      -- the x coordinate of the beacon will be 1 more than the end of the lower of the ranges
      | length truncated == 2 = (,y) . (1 +) . snd . head $ sort truncated
      | otherwise = go ys
      where
        truncated = truncateRanges $ offLimitsRanges sensors y

main :: IO ()
main = do
  let inputFile = "input/day15.txt"
  input <- readFile inputFile
  let Right sensors = runParser (some parseSensor) inputFile input
      illegalCount = offLimitsInRow sensors 2000000
      (bx, by) = findBeacon sensors
      tuningFreq = bx * 4000000 + by
  fprintLn ("Part 1 (no-zones at y=2000000): " % int) illegalCount -- Expected: 4811413
  fprintLn ("Part 2 (tuning frequency): " % int) tuningFreq -- Expected: 13171855019123
