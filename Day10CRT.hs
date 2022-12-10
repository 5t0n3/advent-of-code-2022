{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (liftM)
import Data.List.Split (chunksOf)
import Formatting

data CPU = CPU
  { x :: Int,
    waiting :: Bool,
    currentCycle :: Int,
    xHistory :: [Int],
    instructions :: [String],
    signalStrengths :: [Int]
  }

interestingCycles :: [Int]
interestingCycles = [20, 60, 100, 140, 180, 220]

initCPU :: [String] -> CPU
initCPU instructions =
  CPU
    { x = 1,
      waiting = False,
      currentCycle = 1,
      xHistory = [],
      instructions = instructions,
      signalStrengths = []
    }

main :: IO ()
main = do
  input <- liftM lines . readFile $ "input/day10.txt"
  let CPU {signalStrengths = strengths, xHistory} = execute (initCPU input)
      -- pair the value in the X register at each clock cycle with the corresponding CRT pixel location
      synced = map (zip [0 ..]) . chunksOf 40 . reverse $ xHistory
      display = unlines . map (map toPixel) $ synced
  fprintLn ("Part 1 (sum of interesting signal strengths): " % int) (sum strengths) -- Expected: 14060
  putStrLn "Part 2 (CRT display):"
  putStr display -- Expected: PAPKFKEJ

-- sprite -> location of sprite center, crt -> location of crt pixel
-- since the sprite is 3 pixels wide, crt has to be within one pixel of its center to be rendered
toPixel :: (Int, Int) -> Char
toPixel (crt, sprite) = if abs (crt - sprite) <= 1 then '#' else '.'

execute :: CPU -> CPU
execute cpu@CPU {instructions = []} = cpu
execute cpu@CPU {x = oldX, waiting, currentCycle, xHistory, instructions = (instruction : rest), signalStrengths}
  | instr == "noop" = execute advanceInstr
  -- instr == "addx" is implied
  | not waiting = execute startAdd
  -- done waiting to perform addx
  | otherwise = execute addResult
  where
    (instr, arg) = splitAt 5 instruction
    baseUpdate =
      cpu
        { currentCycle = currentCycle + 1,
          xHistory = oldX : xHistory,
          signalStrengths =
            if currentCycle `elem` interestingCycles
              then oldX * currentCycle : signalStrengths
              else signalStrengths
        }
    advanceInstr = baseUpdate {instructions = rest}
    -- instruction isn't advanced until the second cycle of addx
    startAdd = baseUpdate {waiting = True}
    addResult = advanceInstr {x = oldX + read arg, waiting = False}
