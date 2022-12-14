{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad (liftM)
import Data.Array ((!), (//))
import qualified Data.Array as A
import Data.List (groupBy, intersperse, minimumBy, nub, sortBy)
import Data.Tuple (swap)
import Data.Void (Void)
import Formatting (fprintLn, int, string, (%))
import Text.Megaparsec
import Text.Megaparsec.Char (char, eol)
import Text.Megaparsec.Char.Lexer (decimal)

type Point = (Int, Int)

type Parser = Parsec Void String

type Cave = A.Array Point Char

parseLine :: Parser [Point]
parseLine = parsePoint `sepBy` chunk " -> " <* eol

parsePoint :: Parser Point
parsePoint = do
  first <- decimal
  char ','
  second <- decimal
  return (first, second)

rockPoints :: [Point] -> [Point]
rockPoints [] = []
rockPoints [_] = []
rockPoints (first : second : rest) = tupleRange first second ++ rockPoints (second : rest)

tupleRange (x1, y1) (x2, y2) = [(x, y) | x <- range x1 x2, y <- range y1 y2]
  where
    range a b = if a <= b then [a .. b] else [b .. a]

findBounds :: [[Point]] -> (Point, Point)
findBounds groups = (topRight, bottomLeft)
  where
    (xs, ys) = unzip $ concat groups
    topRight = (minimum xs, 0)
    bottomLeft = (maximum xs, maximum ys)

buildCave :: [[Point]] -> Cave
buildCave groups = A.array bounds pointList
  where
    bounds = findBounds groups
    rocks = nub . concat $ map rockPoints groups
    toChar point =
      if point `elem` rocks
        then '#'
        else
          if point == (500, 0)
            then '+'
            else '.'
    pointList = [(point, toChar point) | point <- A.range bounds]

showCave :: Cave -> String
showCave cave = concat $ intersperse "\n" lines
  where
    listForm = sortBy (mapYs compare) $ A.assocs cave
    lines = map (map snd) $ groupBy (mapYs (==)) listForm
    mapYs f ((_, y1), _) ((_, y2), _) = f y1 y2

findSandStart :: Cave -> Point
findSandStart cave = (x, y - 1)
  where
    occupied = map fst . filter ((`elem` ['#', 'o']) . snd) $ A.assocs cave
    inDropColumn = filter ((== 500) . fst) occupied
    (x, y) = minimumBy (compare . swap) inDropColumn

addSand :: Cave -> Cave
addSand cave
  | Right pos <- finalPos = cave // [(pos, 'o')]
  | Left _ <- finalPos = cave
  where
    startPos = findSandStart cave
    finalPos = settleSand cave startPos

settleSand :: Cave -> Point -> Either () Point
settleSand cave point@(x, y)
  -- falling will go out of bounds (part 1 stopping point)
  | any (not . (inBounds cave)) nextCandidates = Left ()
  -- sand drop spot blocked (part 2 stopping point)
  | inBounds cave point && cave ! point == '#' = Left ()
  -- can't drop any lower
  | null validNext = Right point
  -- never settle for less :)
  | otherwise = settleSand cave next
  where
    nextCandidates = map (,y + 1) [x, x - 1, x + 1]
    validNext = filter (\p -> cave ! p == '.') nextCandidates
    next = head validNext

inBounds :: Cave -> Point -> Bool
inBounds cave = A.inRange (A.bounds cave)

stableState :: Cave -> Cave
stableState cave
  | newSand == cave = cave
  | otherwise = stableState newSand
  where
    newSand = addSand cave

expandFloor :: Cave -> Cave
expandFloor cave = A.array newBounds [(point, toChar point) | point <- A.range newBounds]
  where
    newBounds = newFloorBounds cave
    floorLevel = snd (snd newBounds)
    toChar point =
      if inBounds cave point
        then cave ! point
        else
          if snd point == floorLevel
            then '#'
            else '.'

newFloorBounds :: Cave -> (Point, Point)
newFloorBounds cave = ((xMin, yMin), (xMax, floorLevel))
  where
    ((_, yMin), (_, yMax)) = A.bounds cave
    floorLevel = yMax + 2
    -- triangle width is twice its height plus 1
    (xMin, xMax) = (500 - floorLevel, 500 + floorLevel)

sandCount :: Cave -> Int
sandCount = length . filter (== 'o') . A.elems

main :: IO ()
main = do
  let inputFile = "day14.txt"
  input <- readFile inputFile
  let Right lineGroups = parse (some parseLine) inputFile input
      -- part 1
      cave = buildCave lineGroups
      sanded = stableState cave
      -- part 2
      expanded = expandFloor sanded
      -- note: it takes forever to find this stable state since there's so much more sand lol
      stableExpanded = stableState expanded
  fprintLn ("Cave: \n" % string) (showCave stableExpanded)
  fprintLn ("Part 1 (sand at rest): " % int) (sandCount sanded) -- Expected: 1072
  fprintLn ("Part 2 (oh yeah floor): " % int) (sandCount stableExpanded) -- Expected: 24659
