{-# LANGUAGE OverloadedStrings #-}

import Data.List.Split (divvy)
import qualified Data.Set as S
import Formatting

main :: IO ()
main = do
  input <- readFile "input/day06.txt"
  let packetStart = findStart 4 input -- Expected: 1300
      messageStart = findStart 14 input -- Expected: 3986
  fprintLn ("Part 1 result (packet start): " % int) packetStart
  fprintLn ("Part 2 result (message start): " % int) messageStart

findStart :: Int -> String -> Int
findStart length = startPos length length . map S.fromList . divvy length 1

-- sets are deduplicated by definition so all that needs to be checked is their size
startPos :: Int -> Int -> [S.Set Char] -> Int
startPos len pos (candidate : rest) = if S.size candidate == len then pos else startPos len (pos + 1) rest
