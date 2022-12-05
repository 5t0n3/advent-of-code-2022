{-# LANGUAGE OverloadedStrings #-}

import Data.IntMap ((!))
import qualified Data.IntMap as I
import Data.List (foldl')
import Data.List.Split (chunksOf, splitOn)
import Data.Maybe (fromMaybe)
import Formatting

type Stacks = I.IntMap String

type Move = (Int, Int, Int)

main :: IO ()
main = do
  input <- readFile "input/day05.txt"
  -- input <- readFile "day5.txt"
  let [crateLayout, moveText] = map lines . splitOn "\n\n" $ input
      -- omit the stack numbers
      stacksString = init crateLayout
      -- break up crates into (column, label) tuples (label includes following space)
      rawCrates = map (zip [1 ..] . chunksOf 4) stacksString
      stacks = foldr parseCrates I.empty rawCrates
      moves = map parseMove moveText
      part1Top = topCrates moveP1 stacks moves
      part2Top = topCrates moveP2 stacks moves
  fprintLn ("Part 1 (top crates): " % string) part1Top -- Expected: PTWLTDSJV
  fprintLn ("Part 2 (top crates - order preserved): " % string) part2Top -- Expected: WZMFVGGZP
  where
    topCrates moveFunc stacks = foldr ((:) . head) "" . foldl' moveFunc stacks

parseCrates :: [(Int, String)] -> Stacks -> Stacks
parseCrates [] stacks = stacks
parseCrates (pair : rest) stacks
  -- empty "label" -> no crate in column
  | label == ' ' = parseCrates rest stacks
  | otherwise = parseCrates rest updated
  where
    (col, crate) = pair
    label = crate !! 1
    -- you could probably also use I.insertWith but this feels more ergonomic imo
    updated = I.alter (Just . (label :) . fromMaybe []) col stacks

-- part 1 move (1 at a time -> order is reversed)
moveP1 :: Stacks -> Move -> Stacks
moveP1 stacks (amount, start, end) = I.adjust (reverse moved ++) end (I.insert start newStart stacks)
  where
    (moved, newStart) = splitAt amount (stacks ! start)

-- part 2 move (all at once -> order is preserved)
moveP2 :: Stacks -> Move -> Stacks
moveP2 stacks (amount, start, end) = I.adjust (moved ++) end (I.insert start newStart stacks)
  where
    (moved, newStart) = splitAt amount (stacks ! start)

parseMove :: String -> Move
parseMove line = (read amount, read start, read end)
  where
    [_, amount, _, start, _, end] = splitOn " " line
