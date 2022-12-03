{-# LANGUAGE OverloadedStrings #-}

import Data.List (foldl', foldl1')
import Data.List.Split (chunksOf)
import qualified Data.Set as S
import Formatting

main :: IO ()
main = do
  input <- readFile "input/day03.txt"
  let rucksacks = lines input
      duplicateSum = prioritySum . map splitHalfway $ rucksacks -- Expected: 8298
      badgeSum = prioritySum . chunksOf 3 $ rucksacks -- Expected: 2708
  fprintLn ("Part 1 (sum of duplicate element priorities): " % int) duplicateSum
  fprintLn ("Part 2 (sum of badge priorities): " % int) badgeSum
  where
    priorityAccumulator = flip ((+) . itemPriority . commonItem)
    prioritySum = foldl' priorityAccumulator 0

-- Numbers were chosen to make A map to 27 and a map to 1; everything else follows in ASCII :)
itemPriority :: Char -> Int
itemPriority item = mod (fromEnum item - 38) 58

-- Essentially splitAt but more convenient for reuse due to not returning a tuple
splitHalfway :: String -> [String]
splitHalfway line = [take halfLen line, drop halfLen line]
  where
    halfLen = div (length line) 2

-- This works because there should only be 1 common element in all cases
commonItem :: [String] -> Char
commonItem = S.elemAt 0 . foldl1' S.intersection . map S.fromList
