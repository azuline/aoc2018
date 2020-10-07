{-# LANGUAGE NumericUnderscores #-}

import qualified Data.List       as List
import           Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import qualified System.IO       as IO

type Number     = Int
type SumNumber  = Int
type HasPlant   = Bool
type Pot        = (Number, HasPlant)
type State      = [Pot]
type Pattern    = [Bool]
type Rules      = Map.Map Pattern HasPlant
type Generation = Int

main :: IO ()
main = do
    contents <- IO.readFile "../inputs/day12.txt"
    let (initialState, rules) = parseInput contents

    putStrLn $ "Part 1: " ++ show (part1 rules initialState)
    putStrLn $ "Part 2: " ++ show (part2 rules initialState)

-- Parse out a tuple of initial state and rules from the input.
parseInput :: String -> (State, Rules)
parseInput input = (parseState stateInput, parseRules rulesInput)
    where lines'     = lines input
          stateInput = head lines'
          rulesInput = drop 2 lines'

parseState :: String -> State
parseState input = zip [0..] $ List.map (== '#') $ drop 15 input

parseRules :: [String] -> Rules
parseRules input = Map.fromList list
    where list = List.map (\i -> (List.map (== '#') $ take 5 i, last i == '#')) input

-- Return the sum of the integers.
part1 :: Rules -> State -> SumNumber
part1 rules state = sumPotsWithPlants twentyGenerations
    where twentyGenerations = List.foldl' (\ acc _ -> emulateGeneration rules acc) state [1..20]

-- Emulate a single generation of plant growth.
--
-- Note: Before emulating, we "expand" the state to include two extra no-plant
-- pots on the left and right. And after emulating, we "trim" the state to
-- remove all no-plant pots on the left and right.
emulateGeneration :: Rules -> State -> State
emulateGeneration rules = trimState . emulateState rules . expandState

-- Add two extra no-plant pots to the left and right of the state.
expandState :: State -> State
expandState state =
    [(min' - 2, False), (min' - 1, False)] ++ state ++ [(max' + 1, False), (max' + 2, False)]
    where min' = fst $ head state
          max' = fst $ last state

-- Convert the current generation of pots in the state to the next generation of pots.
emulateState :: Rules -> State -> State
emulateState rules state = emulateStateAux rules state False False

emulateStateAux :: Rules -> State -> HasPlant -> HasPlant -> State
emulateStateAux _     []                     _     _     = []
emulateStateAux rules ((num, hasPlant):pots) left2 left1 =
    (num, newHasPlant):emulateStateAux rules pots left1 hasPlant
    where newHasPlant = rules ! [left2, left1, hasPlant, right1, right2]
          (right1, right2) = case pots of
              []        -> (False, False)
              [one]     -> (snd one, False)
              one:two:_ -> (snd one, snd two)

-- Trim the no-plant pots from the left and right of the state.
trimState :: State -> State
trimState = List.dropWhile hasNoPlant . List.dropWhileEnd hasNoPlant
    where hasNoPlant = not . snd

-- Sum the numbers of the pots with plants in them.
sumPotsWithPlants :: State -> SumNumber
sumPotsWithPlants = List.foldl' foldF 0
    where foldF acc (num, hasPlant) = if hasPlant then acc + num else acc

-- Eventually these plants will become sparse to the point where the same
-- pattern is just repeated over and over again, except the plant numbers march
-- to the right once per generation.
--
-- How do I know this? I ran part 1 for 200 generations and observed.
--
-- So we find this generation in which the the plant numbers start marching and
-- then just speed-up the remainder of the 50 billion generations.
part2 :: Rules -> State -> SumNumber
part2 rules state = sumPotsWithPlants $ fiftyBillionGenerations rules state

fiftyBillionGenerations :: Rules -> State -> State
fiftyBillionGenerations rules state = List.map speedUpPot marchingState
    where (marchingState, generation) = findMarchingGeneration rules state 0 []
          generationsLeft             = 50_000_000_000 - generation
          speedUpPot (num, hasPlant)  = (num + generationsLeft, hasPlant)

findMarchingGeneration :: Rules -> State -> Generation -> State -> (State, Generation)
findMarchingGeneration rules state generation prevState
    | isMarchingGeneration prevState state = (state, generation)
    | otherwise = findMarchingGeneration rules nextState (generation + 1) state
    where nextState = emulateGeneration rules state

isMarchingGeneration :: State -> State -> Bool
isMarchingGeneration prev curr = currLen == prevLen && List.all allF (zip prev curr)
    where prevLen = length prev
          currLen = length curr
          allF ((num1, has1), (num2, has2)) = num2 == num1 + 1 && has1 == has2
