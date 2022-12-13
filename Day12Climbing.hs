{-# LANGUAGE OverloadedStrings, TupleSections #-}
{- very heavy inspiration taken from https://github.com/bereal/AdventOfCodeHaskell/blob/main/src/Year2022/Day12.hs -}

import qualified Data.Array as A
import qualified Data.Set as S
import Data.Array ((!))
import Data.Char (ord)
import Formatting (fprintLn, int, (%))

type Point = (Int, Int)

type Map = A.Array Point Char

toMap :: [[Char]] -> Map
toMap xs =
    let (h, w) = (length xs, length $ head xs)
        in A.listArray ((0, 0), (h - 1, w - 1)) $ concat xs

height :: Char -> Int
height 'S' = 1
height 'E' = 26
height c = ord c - ord 'a' + 1

find :: Char -> Map -> Point
find c = fst . head . filter ((==c) . snd) . A.assocs

adjacent :: Map -> Point -> [Point]
adjacent g n@(x,y) = let
        v = height $ g ! n
        neighbors = filter (A.inRange $ A.bounds g) [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
    in filter (\n -> v - height (g ! n) <= 1) neighbors

bfs' :: S.Set Point -> [(Point, Int)] -> Map -> Char -> Int
bfs' seen ((start, dist):rest) g end
  | g ! start == end = dist
  | S.member start seen = bfs' seen rest g end
  | otherwise = let neighbours = adjacent g start
                    seen' = S.insert start seen
                    new = filter (`S.notMember` seen) neighbours
                    rest' = rest ++ map (,dist+1) new
                in bfs' seen' rest' g end

bfs :: Map -> Char -> Char -> Int
bfs heightmap start end = let startCoord = find start heightmap in bfs' S.empty [(startCoord, 0)] heightmap end

main :: IO ()
main = do
  input <- readFile "input/day12.txt"
  let splitInput = lines input
      heights = toMap splitInput
      part1 = bfs heights 'E' 'S'
      part2 = bfs heights 'E' 'a'
  fprintLn ("Part 1 (shortest path to start): " % int) part1 -- Expected: 468
  fprintLn ("Part 2 (shortest path to 'a'): " % int) part2 -- Expected: 459
