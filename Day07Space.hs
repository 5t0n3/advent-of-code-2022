{-# LANGUAGE OverloadedStrings #-}

import Data.List (foldl', stripPrefix)
import qualified Data.List.Split as S
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Formatting

data Entry = Directory String | File String Int deriving (Show)

-- TODO: parentDirs is redundant lmao
type ParentDirs = M.Map String String

type Children = M.Map String [Entry]

data State = State
  { currentDir :: String,
    parents :: ParentDirs,
    children :: Children
  }
  deriving (Show)

defaultState :: State
defaultState =
  State
    { currentDir = "/",
      parents = M.empty,
      children = M.empty
    }

main :: IO ()
main = do
  input <- readFile "input/day07.txt"
  -- ignore the initial "cd /"
  -- note: duplicate folders </3
  let splitCommands = tail . map (splitNoBlanks "\n") . splitNoBlanks "$ " $ input
      State {parents = par, children = chl} = evalCommands splitCommands defaultState
      directorySizes = M.fromList . map (\dir -> (dir, directorySize chl dir)) $ M.keys chl
      smallDirectoryTotal = M.foldl' (\sum size -> if size <= 100000 then size + sum else sum) 0 directorySizes -- Expected: 1501149
      -- PART 2: min to free
      totalUsedSpace = directorySizes ! "/"
      currentFreeSpace = 70000000 - totalUsedSpace
      minToFree = 30000000 - currentFreeSpace
      minActualFree = minimum . filter (> minToFree) . M.elems $ directorySizes -- Expected: 10096985
  fprintLn ("Part 1 (smol directory size sum): " % int) smallDirectoryTotal
  fprintLn ("Part 2 (min to free while enough): " % int) minActualFree
  where
    -- essentially splitOn with dropBlanks added
    splitNoBlanks = S.split . S.dropBlanks . S.dropDelims . S.onSublist

evalCommands :: [[String]] -> State -> State
evalCommands [] state = state
evalCommands (cmd : rest) state
  | command == "cd" = evalCommands rest cdState
  | command == "ls" = evalCommands rest lsState
  | otherwise = error ("Unexpected command: " ++ command)
  where
    (command : args) = S.splitOn " " (head cmd)
    cmdResult = tail cmd
    targetDir = case head args of
      ".." -> (parents state) ! (currentDir state)
      dir -> withParents (currentDir state) dir
    lsOutput = parseLsOutput cmdResult (currentDir state)
    newChildren = M.insert (currentDir state) lsOutput (children state)
    lsState =
      state
        { children = newChildren,
          parents = updateParents lsOutput (currentDir state) (parents state)
        }
    cdState = state {currentDir = targetDir}

directorySize :: Children -> String -> Int
directorySize childMap dirName = foldl' (flip ((+) . childSize)) 0 $ childMap ! dirName
  where
    childSize (File _ size) = size
    childSize (Directory name) = directorySize childMap name

parseLsOutput :: [String] -> String -> [Entry]
parseLsOutput [] _ = []
parseLsOutput (current : rest) currentDir = currentEntry : parseLsOutput rest currentDir
  where
    currentEntry = case stripPrefix "dir " current of
      Just dir -> Directory $ withParents currentDir dir
      Nothing ->
        let [size, name] = S.splitOn " " current
         in File name (read size)

updateParents :: [Entry] -> String -> ParentDirs -> ParentDirs
updateParents ((Directory name) : rest) currentDir parents = updateParents rest currentDir (M.insert name currentDir parents)
updateParents (_ : rest) currentDir parents = updateParents rest currentDir parents
updateParents [] _ parents = parents

withParents :: String -> String -> String
withParents currentDir name = currentDir ++ name ++ "/"
