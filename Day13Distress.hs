{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (liftM)
import Data.Char (digitToInt)
import Data.List (elemIndex, foldl', singleton, sort)
import Data.List.Split (splitOn)
import Formatting (fprintLn, int, (%))
import Text.Parsec

data Segment = List [Segment] | Number Int deriving (Show, Eq)

-- Haskell's list comparisons already work like we need which is really nice
instance Ord Segment where
  compare (List left) (List right) = compare left right
  compare (Number left) (Number right) = compare left right
  compare left right = compare (toList left) (toList right)

toList :: Segment -> [Segment]
toList (List list) = list
toList num@(Number _) = singleton num

dividerPackets :: [Segment]
dividerPackets = map parseSegment ["[[2]]", "[[6]]"]

main :: IO ()
main = do
  input <- readFile "input/day13.txt"
  let segments = map parseSegment . words $ input
      correctOrderList = zip [1 ..] . map (uncurry (<)) . pairs $ segments
      indexSum = sum . map fst . filter snd $ correctOrderList
      sortedWithDividers = sort $ dividerPackets ++ segments
      decoderKey =
        product
          . map
            ( \packet ->
                -- again, this is probably a bad idea normally but works in this case :)
                let Just index = elemIndex packet sortedWithDividers
                 in index + 1
            )
          $ dividerPackets
  fprintLn ("Part 1 result (correct order index sum): " % int) indexSum -- Expected: 6656
  fprintLn ("Part 2 result (decoder key): " % int) decoderKey -- Expected: 19716

parseList :: Parsec String st Segment
parseList = liftM List $ char '[' *> sepBy (parseList <|> parseNum) (char ',') <* char ']'

-- inspiration: https://stackoverflow.com/a/10726784
parseNum :: Parsec String st Segment
parseNum = Number . foldl' (\a i -> a * 10 + digitToInt i) 0 <$> many1 digit

-- This is probably not how you're supposed to use parsec but it works for me ¯\_(ツ)_/¯
parseSegment :: String -> Segment
parseSegment str = let Right res = parse parseList "" str in res

-- I realize this is a partial function but that doesn't really matter in this case
pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (a1 : a2 : rest) = (a1, a2) : pairs rest
