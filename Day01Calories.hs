{-# LANGUAGE OverloadedStrings #-}

import Data.List (sort)
import Data.List.Split (splitOn)
import Formatting

main :: IO ()
main = do
  input <- readFile "input/day01.txt"
  -- splitOn [""] groups the calories by elf (it essentially splits on double newlines)
  let caloriesList = map sum . map (map read) . splitOn [""] . lines $ input
      maxCalories = maximum caloriesList -- expected output: 68292
      top3 = sum . take 3 . reverse . sort $ caloriesList -- expected output: 203203
  fprintLn ("Part 1 (top calorie amount): " % int) maxCalories
  fprintLn ("Part 2 (top 3 elves' calories): " % int) top3
