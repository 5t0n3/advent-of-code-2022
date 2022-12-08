{-# LANGUAGE OverloadedStrings #-}

import Data.List (transpose)
import Data.List.Split (endBy)
import Formatting

main :: IO ()
main = do
  input <- readFile "input/day08.txt"
  -- SETUP: we transpose the rows into columns to allow for using the same functions for both parts
  let rows = endBy "\n" input
      cols = transpose rows

      -- PART 1
      -- concat is used to flatten lists to make them nicer to map over later
      rowVisibilities = concat . mapNested visibleInRow $ rows
      -- columns must be transposed again to line up with rows
      colVisibilities = concat . transpose . mapNested visibleInRow $ cols
      -- a tree only needs to be visible in a row _or_ a column, not necessarily both
      allVisibilities = zipWith (||) rowVisibilities colVisibilities
      -- count all of the Trues to get the number of trees visible
      visibleTotal = length . filter id $ allVisibilities -- Expected: 1717

      -- PART 2
      rowScores = concat . mapNested lineScore $ rows
      colScores = concat . transpose . mapNested lineScore $ cols
      netScores = zipWith (*) rowScores colScores
      maxScore = maximum netScores -- Expected: 321975
  fprintLn ("Part 1 (number of trees visible from outside forest): " % int) visibleTotal
  fprintLn ("Part 2 (max visibility score): " % int) maxScore
  where
    mapIndices func line = map (func line) [0 .. length line - 1]
    mapNested func = map (mapIndices func)

visibleInRow :: String -> Int -> Bool
visibleInRow row index
  -- all trees on the perimeter are visible by definition
  | index == 0 || index == length row - 1 = True
  -- inner trees only have to be visible from one side
  | otherwise = all (< height) before || all (< height) after
  where
    height = row !! index
    (before, _ : after) = splitAt index row

lineScore :: String -> Int -> Int
lineScore line index
  -- nobody likes trees on the perimeter
  | index == 0 || index == length line - 1 = 0
  | otherwise = sideScore (reverse before) * sideScore after
  where
    height = line !! index
    (before, _ : after) = splitAt index line
    -- `signum` accounts for span (well, takeWhile) not including the element that doesn't
    -- satisfy the predicate while also taking into account if we hit an edge
    sideScore sideList =
      let (visible, invisible) = span (< height) sideList
       in length visible + signum (length invisible)
