{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict ((!))
import Data.List (foldl', permutations)
import Data.Void (Void)
import Debug.Trace (trace)
import Formatting (fprintLn, shown, int, (%))
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Valve = Valve {
  name :: String,
  flowRate :: Int,
  tunnels :: [String]
} deriving (Show)

data FlowState = FlowState {
  totalFlow :: Int,
  openRate :: Int,
  timeLeft :: Int
} deriving (Show)

type TunnelSystem = HM.HashMap String [String]
-- from, to
type TunnelDists = HM.HashMap (String, String) Int

type Parser = Parsec Void String

parseValve :: Parser Valve
parseValve = do
  string "Valve "
  name <- some upperChar
  string " has flow rate="
  flowRate <- L.decimal
  string "; "
  -- plurals are annoying ok
  string "tunnel leads to valve " <|> string "tunnels lead to valves "
  tunnels <- some upperChar `sepBy` string ", "
  eol
  return $ Valve { name, flowRate, tunnels }

distancesFrom :: TunnelSystem -> [String] -> String -> TunnelDists
distancesFrom system targets valve = go HM.empty (HS.singleton valve) HS.empty 0

  where
    go targetDists candidates visited currentDistance
      -- found all targets
      | null missing = HM.mapKeys (valve,) targetDists
      -- at some target nodes
      | not $ null currentTargets = go updatedDists parents newVisited (currentDistance + 1)
      -- not at any interesting valves
      | otherwise = go targetDists parents newVisited (currentDistance + 1)
      where
        missing = filter (not . (`HM.member` targetDists)) targets
        currentTargets = HS.filter (`elem` targets) candidates
        updatedDists = foldl' (\acc t -> HM.insert t currentDistance acc) targetDists currentTargets
        parents = HS.fromList . filter (not . (`HS.member` visited)) $ concatMap (system !) candidates
        newVisited = HS.union candidates visited

calcFlow :: TunnelDists -> [Valve] -> FlowState -> Int
-- determine travel distance
calcFlow dists (v1:vs@(v2:_)) flow@(FlowState {totalFlow, openRate, timeLeft})
  -- not enough time to open valve
  | travelOpenTime > timeLeft = nextCalc $ tickFor timeLeft
  -- travel to and open next valve
  | otherwise = nextCalc $ (tickFor travelOpenTime) { openRate = openRate + flowRate v2 }
  where
    -- it takes 1 minute to open a valve after travelling to it
    -- travelOpenTime = trace ("open rate: " ++ show openRate) $ dists ! (name v1, name v2) + 1
    travelOpenTime = dists ! (name v1, name v2) + 1
    tickFor = tickFlowState flow
    nextCalc = calcFlow dists vs

-- just vibe until time runs out (all are open)
calcFlow _ _ flow@(FlowState {timeLeft}) = totalFlow $ tickFlowState flow timeLeft

tickFlowState :: FlowState -> Int -> FlowState
tickFlowState flow@(FlowState{totalFlow, openRate, timeLeft}) time = flow { totalFlow = totalFlow + openRate * time, timeLeft = timeLeft - time }

findFlow :: TunnelDists -> [Valve] -> Int
findFlow dists path = calcFlow dists path $ FlowState { totalFlow = 0, openRate = 0, timeLeft = 30 }

main :: IO ()
main = do
  let inputFile = "input/day16.txt"
  input <- readFile inputFile
  let Right valves = runParser (some parseValve) inputFile input
      workingValves = filter ((>0) . flowRate) valves
      workingNames = map name workingValves
      valveSystem = HM.fromList $ map (\v -> (name v, tunnels v)) valves
      nameMap = HM.fromList $ map (\v -> (name v, v)) valves
      workingDistances = foldl' (\acc v -> acc `HM.union` distancesFrom valveSystem workingNames v) HM.empty ("AA" : workingNames)
      -- exFlow = findFlow workingDistances (map (nameMap !) ["AA", "DD", "BB", "JJ", "HH", "EE", "CC"])
      startValve = nameMap ! "AA"
      maxFlow = foldl (\acc path -> max acc $ findFlow workingDistances path) 0 $ map ((startValve:) . map (nameMap !)) (permutations (workingNames))
  fprintLn ("Part 1 result (most flow): " % int) maxFlow
  --fprintLn ("Example flow? " % shown) exFlow
