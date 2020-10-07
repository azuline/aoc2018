import qualified Data.List       as List
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import qualified System.IO       as IO

type Dep     = (Char, Char)
type DepMap  = Map.Map Char [Char]
type Worker  = (Char, Int)

upperCase = Set.fromList ['A'..'Z']
charToTime = Map.fromList $ zip ['A'..'Z'] [60..]

main :: IO ()
main = do
    contents <- IO.readFile "../inputs/day07.txt"
    let deps    = [ parseDeps line | line <- lines contents ]
        depsMap = constructDepsMap deps

    putStrLn $ "Part 1: " ++ show (part1 depsMap)
    putStrLn $ "Part 2: " ++ show (part2 depsMap)

-- Parse a tuple of dependency chars. Drop the `S` in step and then parse out
-- the first two uppercase letters to be the deps.
parseDeps :: String -> Dep
parseDeps line = (from, to)
    where [from, to] = take 2 $ List.filter (`Set.member` upperCase) $ drop 1 line

-- Construct a mapping of a step to the steps that must be completed before it.
constructDepsMap :: [Dep] -> DepMap
constructDepsMap deps = depsMap
    where withTosInserted = mapTosToFroms deps Map.empty
          depsMap = insertFroms deps withTosInserted

-- Given a list of `(from, to)` tuples, map `to` to `[from]`.
mapTosToFroms :: [Dep] -> DepMap -> DepMap
mapTosToFroms []                map = map
mapTosToFroms ((from, to):deps) map = mapTosToFroms deps $ Map.alter alterF to map
    where alterF (Just froms) = Just (from:froms)
          alterF Nothing      = Just [from]

-- Given a map and a list of `(from, to)`, if `from` doesn't exist in the set
-- of keys in the map, insert it with an empty string value.
insertFroms :: [Dep] -> DepMap -> DepMap
insertFroms []               map = map
insertFroms ((from, _):deps) map = insertFroms deps $ Map.alter alterF from map
    where alterF (Just value) = Just value
          alterF Nothing      = Just ""

-- Find the lowest-ordered step with no dependencies, construct a new map
-- without it, and then recursively construct the rest of the string, using the
-- found step as the current string character.
part1 :: DepMap -> String
part1 map
    | Map.null map = ""
    | otherwise    = stepToDo:part1 mapWithoutStep
    where depsWithNoDependencies = List.map fst $ List.filter (null . snd) $ Map.toList map
          stepToDo               = minimum depsWithNoDependencies
          mapWithoutStep         = Map.map (List.delete stepToDo) $ Map.delete stepToDo map

part2 :: DepMap -> Int
part2 = emulateWorkers []

emulateWorkers :: [Worker] -> DepMap -> Int
emulateWorkers workers map
    | Map.null map = -1 -- Subtract 1 for the extra tick to remove the last finished worker.
    | otherwise    = 1 + emulateWorkers nextWorkers map'
    where finishedWorkers    = List.filter ((== 0) . snd) workers
          unfinishedWorkers  = workers List.\\ finishedWorkers
          numFreeWorkers     = 5 - length unfinishedWorkers
          finishedChars      = List.map fst finishedWorkers
          map'               = removeFinishedFromMap map finishedChars
          workersNextSecond  = List.map (\(c, i) -> (c, i - 1)) unfinishedWorkers
          unfinishedChars    = List.map fst unfinishedWorkers
          newWorkerChars     = take numFreeWorkers $ List.filter (`notElem` unfinishedChars) $ getDepLessChars map'
          newWorkers         = List.map (\c -> (c, charToTime Map.! c)) newWorkerChars
          nextWorkers        = newWorkers ++ workersNextSecond

-- Remove chars from the map.
removeFinishedFromMap :: DepMap -> [Char] -> DepMap
removeFinishedFromMap map chars = (removeFromValues . stripKeys) map
    where removeFromValues = Map.map (List.\\ chars)
          stripKeys map'   = List.foldl' (flip Map.delete) map' chars

-- Get chars from the map which have no dependencies.
getDepLessChars :: DepMap -> [Char]
getDepLessChars map = List.map fst $ List.filter (null . snd) $ Map.toList map
