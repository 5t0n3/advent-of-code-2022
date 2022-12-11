{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Char (digitToInt)
import Data.IntMap.Strict ((!))
import qualified Data.IntMap.Strict as I
import Data.List (foldl', iterate', sortBy, stripPrefix)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Sequence ((><))
import qualified Data.Sequence as S
import Formatting

data Monkey = Monkey
  { items :: S.Seq Int,
    operation :: Int -> Int,
    divisibleBy :: Int,
    trueTarget :: Int,
    falseTarget :: Int,
    inspections :: Int
  }

type Monkeys = I.IntMap Monkey

main :: IO ()
main = do
  input <- readFile "input/day11.txt"
  let rawMonkeys = map lines . splitOn "\n\n" $ input
      monkeys = I.fromList . map parseMonkey $ rawMonkeys
      twentyRounds = iterate' (doRound True) monkeys !! 20
      tenthousandRounds = iterate' (doRound False) monkeys !! 10000
  fprintLn ("Part 1 result (monkey business level after 20 rounds): " % int) $ monkeyBusiness twentyRounds -- Expected: 58786
  fprintLn ("Part 2 result (10000 rounds): " % int) $ monkeyBusiness tenthousandRounds -- Expected: 14952185856
  where
    -- monkey business is the product of the number of items inspected by the two most active monkeys
    monkeyBusiness = product . take 2 . sortBy (flip compare) . map inspections . I.elems

doRound :: Bool -> Monkeys -> Monkeys
doRound divBy3 oldMonkeys = foldl' (takeTurn divBy3 modulus) oldMonkeys $ [0 .. maxMonkey]
  where
    (maxMonkey, _) = I.findMax oldMonkeys
    modulus = product . map divisibleBy . I.elems $ oldMonkeys

takeTurn :: Bool -> Int -> Monkeys -> Int -> Monkeys
takeTurn divBy3 modulus ms currentNum
  | S.null items = ms
  | otherwise = updatedMonkeys
  where
    current@Monkey {items, operation, divisibleBy, trueTarget, falseTarget, inspections} = ms ! currentNum
    currentEmptied = current {items = S.empty, inspections = inspections + S.length items}
    updateWorry =
      if divBy3
        then -- this isn't taken the mod of because dividing by 3 violates...something I think lol
        \w -> operation w `div` 3
        else -- the result of whatever operation mod the product of all of the dividends preserves its "multipleness" for every monkey
        -- we have to wrap around because no one can handle that much stress
        \w -> operation w `mod` modulus
    updatedWorries = fmap updateWorry items
    (true, false) = S.partition (\w -> w `mod` divisibleBy == 0) updatedWorries
    appendItems passed monkey@Monkey {items = oldItems} = monkey {items = oldItems >< passed}
    updatedMonkeys =
      I.insert currentNum currentEmptied
        . I.adjust (appendItems true) trueTarget
        . I.adjust (appendItems false) falseTarget
        $ ms

-- apologies for cursed parsing, I blame the format lol
parseMonkey :: [String] -> (Int, Monkey)
parseMonkey [number, rawItems, rawOp, test, true, false] =
  ( num,
    Monkey
      { items,
        operation,
        divisibleBy = lastNumber test,
        trueTarget = lastNumber true,
        falseTarget = lastNumber false,
        inspections = 0
      }
  )
  where
    strip prefix = fromJust . stripPrefix prefix
    num = digitToInt . head . strip "Monkey " $ number
    items = S.fromList . map read . splitOn ", " . strip "  Starting items: " $ rawItems
    [arg1, op, arg2] = words . strip "  Operation: new = " $ rawOp
    operation = case op of
      -- arg1 should always be "old"
      "+" -> (+ read arg2)
      -- arg1 == arg2 handles the old * old case
      "*" -> if arg1 == arg2 then (\worry -> worry * worry) else (* read arg2)
    lastNumber = read . last . words
