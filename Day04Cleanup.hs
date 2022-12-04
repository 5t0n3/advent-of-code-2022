{-# LANGUAGE OverloadedStrings #-}

import Data.List (foldl')
import Data.List.Split (splitOn)
import qualified Data.Set as S
import Formatting

type IntSet = S.Set Int

main :: IO ()
main = do
  input <- readFile "input/day04.txt"
  let ranges = map (listToTuple . map parseRange . splitOn ",") . lines $ input
      completeOverlaps = count completeOverlap ranges -- Expected: 498
      anyOverlaps = count anyOverlap ranges -- Expected: 859
  fprintLn ("Part 1 (complete overlap): " % int) completeOverlaps
  fprintLn ("Part 2 (any overlap): " % int) anyOverlaps
  where count predicate = foldl' (flip ((+) . fromEnum . predicate)) 0

-- used to make function application more convenient
-- (works because it's only ever used on lists with 2 elements)
listToTuple :: [a] -> (a, a)
listToTuple [a, b] = (a, b)

completeOverlap :: (IntSet, IntSet) -> Bool
completeOverlap (first, second) = first `S.isSubsetOf` second || second `S.isSubsetOf` first

anyOverlap :: (IntSet, IntSet) -> Bool
anyOverlap = not . uncurry S.disjoint

parseRange :: String -> IntSet
parseRange range = S.fromList [(read start)..(read end)]
  where [start, end] = splitOn "-" range
