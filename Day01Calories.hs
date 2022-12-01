{-# LANGUAGE OverloadedStrings #-}

import Data.List (sortBy)
import Data.List.Split (splitOn)
import Formatting

main :: IO ()
main = do
  input <- readFile "input/day01.txt"
  let caloriesList = map (sum . map read . lines) . splitOn "\n\n" $ input
      maxCalories = maximum caloriesList -- expected output: 68292
      -- `sortBy (flip compare)` sorts the list in reverse (i.e. descending order)
      top3Total = sum . take 3 . sortBy (flip compare) $ caloriesList -- expected output: 203203
  fprintLn ("Part 1 (top calorie amount): " % int) maxCalories
  fprintLn ("Part 2 (top 3 elves' calories): " % int) top3Total
