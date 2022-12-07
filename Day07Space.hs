{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.HashMap.Strict ((!))
import qualified Data.HashMap.Strict as M
import Data.List (foldl', stripPrefix, uncons)
import qualified Data.List.Split as S
import Data.Maybe (fromJust)
import Formatting

-- directories are represented as a list of all their components to avoid name collisions
data Entry = Directory [String] | File Int deriving (Show)

-- directory -> children mapping
type Children = M.HashMap [String] [Entry]

data State = State
  { currentDir :: [String],
    children :: Children
  }
  deriving (Show)

baseState :: State
baseState =
  State
    { currentDir = ["/"],
      children = M.empty
    }

main :: IO ()
main = do
  input <- readFile "input/day07.txt"
  -- `tail` ignores the initial "cd /"
  let splitCommands = tail . map (splitNoBlanks "\n") . splitNoBlanks "$ " $ input
      -- PART 1
      State {children} = evalCommands splitCommands baseState
      directorySizes = M.map (directorySize children) children
      smallDirectoryTotal = M.foldl' (+) 0 . M.filter (<= 100000) $ directorySizes -- Expected: 1501149
      -- PART 2
      totalUsedSpace = directorySizes ! ["/"]
      currentFreeSpace = 70000000 - totalUsedSpace
      minToFree = 30000000 - currentFreeSpace
      minAdequateFree = minimum . M.elems . M.filter (>= minToFree) $ directorySizes -- Expected: 10096985
  fprintLn ("Part 1 (size of small directories): " % int) smallDirectoryTotal
  fprintLn ("Part 2 (minimum directory size to delete for enough free space): " % int) minAdequateFree
  where
    -- essentially splitOn with dropBlanks added
    splitNoBlanks = S.split . S.dropBlanks . S.dropDelims . S.onSublist

evalCommands :: [[String]] -> State -> State
evalCommands [] state = state
evalCommands (current : rest) state@State {currentDir = cwd, children}
  | command == "cd" = evalCommands rest cdState
  | command == "ls" = evalCommands rest lsState
  | otherwise = error ("Unexpected command: " ++ command)
  where
    (fullCommand, result) = fromJust . uncons $ current
    (command : args) = S.splitOn " " fullCommand
    -- cd
    targetDir = case head args of
      ".." -> tail cwd
      dir -> dir : cwd
    cdState = state {currentDir = targetDir}
    -- ls
    lsOutput = parseLsOutput result cwd
    newChildren = M.insert cwd lsOutput children
    lsState =
      state
        { children = newChildren
        }

-- Recursively calculates the size of a directory from its children
directorySize :: Children -> [Entry] -> Int
directorySize childMap = foldl' (flip ((+) . childSize)) 0
  where
    childSize (File size) = size
    childSize (Directory name) = directorySize childMap (childMap ! name)

parseLsOutput :: [String] -> [String] -> [Entry]
parseLsOutput [] _ = []
parseLsOutput (current : rest) currentDir = currentEntry : parseLsOutput rest currentDir
  where
    currentEntry = case stripPrefix "dir " current of
      Just dir -> Directory $ dir : currentDir
      Nothing ->
        let [size, _] = S.splitOn " " current
         in File $ read size
