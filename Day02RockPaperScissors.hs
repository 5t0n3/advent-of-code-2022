{-# LANGUAGE OverloadedStrings #-}

import Formatting

{-
Outcome encodings: lose -> 0, draw -> 1, win -> 2
Outcome point values; lose -> 0, draw -> 3, win -> 6

Shape encodings: rock -> 0, paper -> 1, scissors -> 2
Shape point values: rock -> 1, paper -> 2, scissors -> 3

General formula for points from round: outcome * 3 + (shape + 1)

To find your shape given the opponent's shape and outcome, in the above
encoding each shape loses to the shape 1 greater than it, wrapping around
from 2 to 0. As a corrolary, each shape wins against the previous one,
again wrapping around. Obviously each shape draws with the one equal to itself.
The wrapping is modeled really well by addition modulo an integer (3 in this case,
since there's 3 different outcomes/shapes). Denoting the opponent's shape as "o",
the following properties should be true, where the results are the symbol we
should play:

Lose: o + 0 -> o - 1 (mod 3)
Draw: o + 1 -> o (mod 3)
Win: o + 2 -> o + 1 (mod 3)

While these properties aren't technically true, if we subtract 1 (mod 3) from the
outcome/left side they become true. This property can be used to then derive an
equation for the shape we should play given the outcome and opponent's shape:

s ≡ o + (r - 1) (mod 3)

Where s is the shape we play, o is the opponent's shape, and r is the result
(i.e. outcome). Note that modulo addition/subtraction is associative, so the
parentheses don't actually matter.

This provides us a solution to part 2.

For part 1, we need to invert this relationship, solving for r in terms of s
and o:

s ≡ o + r - 1 (mod 3)
s + 1 - o ≡ r (mod 3)
r ≡ s + 1 - o (mod 3)

In both parts, the above formula for the points scored in a round can be used
once your shape and the outcome of the game are known.
-}

main :: IO ()
main = do
  input <- readFile "input/day02.txt"
  let parsed = map parseLine . lines $ input
      strategyScore = sum . map (uncurry part1Result) $ parsed -- Expected: 11767
      realScore = sum . map (uncurry part2Result) $ parsed -- Expected: 13886
  fprintLn ("Part 1 (strategy score): " % int) strategyScore
  fprintLn ("Part 2 (real strategy score): " % int) realScore

-- 0 -> rock/lose, 1 -> paper/draw, 2 -> scissors/win
-- In ASCII, 65 is "A" and 88 is "X"
-- The mod 23 (88 - 65 = 23) makes it so X-Z fall in the 0-2 range as well
parseLetter :: Char -> Int
parseLetter c = mod (fromEnum c - 65) 23

parseLine :: String -> (Int, Int)
parseLine (opp : _ : code : []) = (parseLetter opp, parseLetter code)

-- addition mod 3
infixl 6 +.

(+.) :: Int -> Int -> Int
a +. b = mod (a + b) 3

-- subtraction mod 3
infixl 6 -.

(-.) :: Int -> Int -> Int
a -. b = mod (a - b) 3

part1Result :: Int -> Int -> Int
part1Result opponent you = outcome * 3 + (you + 1)
  where
    outcome = you +. 1 -. opponent

part2Result :: Int -> Int -> Int
part2Result opponent outcome = outcome * 3 + (you + 1)
  where
    you = opponent +. (outcome -. 1)
