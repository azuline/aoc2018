{-# LANGUAGE TupleSections #-}

import qualified Data.List       as List
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import qualified Data.Ord        as Ord
import qualified System.IO       as IO

type Guard = Int
type Minute = Int
type Entry = (Minute, String)
type SleepMap = Map Guard [Minute]
type MinuteMap = Map Guard Minute

main :: IO ()
main = do
    contents <- IO.readFile "../inputs/day04.txt"
    let sleepMap = guardsToMinutes contents

    putStrLn $ "Part 1: " ++ show (part1 sleepMap)
    putStrLn $ "Part 2: " ++ show (part2 sleepMap)

-- Create a map of guard IDs to a list of minutes they are awake.
guardsToMinutes :: String -> SleepMap
guardsToMinutes = go Map.empty 0 . splitRecords . List.sort . lines
    where go sleepMap guard [] = sleepMap
          go sleepMap guard ((min1, "falls asleep"):(min2, "wakes up"):entries) =
              let sleepMap' = Map.insertWith (++) guard [min1..min2 - 1] sleepMap
              in  go sleepMap' guard entries
          go sleepMap guard ((_, text):entries) =
              let guard' = read . takeWhile (/= ' ') . drop 7 $ text
              in  go sleepMap guard' entries

-- Split a record into a tuple of the timestamp and the entry.
splitRecords :: [String] -> [Entry]
splitRecords = map (\x -> (read . drop 15 . take 17 $ x, drop 19 x))

-- Find sleepiest guard id * their sleepiest minute
part1 :: SleepMap -> Int
part1 sleepMap = sleepiestGuard * sleepiestMinute
    where sleepiestGuard =
              fst . List.maximumBy (Ord.comparing $ length . snd) . Map.assocs $ sleepMap
          sleepiestMinute = findSleepiestMinute $ sleepMap ! sleepiestGuard

-- Return the most commonly-occurring minute in a list of minutes.
findSleepiestMinute :: [Minute] -> Minute
findSleepiestMinute = fst . List.maximumBy (Ord.comparing snd) . Map.toList . buildFreqMap

-- Build a frequency map from a list of elements.
buildFreqMap :: (Ord a) => [a] -> Map a Int
buildFreqMap = Map.fromListWith (+) . map (,1)

-- Find guard asleep the most on the same minute and multiply their ID by their sleepiest
-- minute.
part2 :: SleepMap -> Int
part2 sleepMap = sleepiestGuard * (sleepiestMinutes ! sleepiestGuard)
    where sleepiestMinutes = Map.map findSleepiestMinute sleepMap
          sleepiestGuard =
              fst
              . List.maximumBy (Ord.comparing $ getSleepiestMinutesCount sleepiestMinutes)
              $ Map.assocs sleepMap

-- Return the times that the guard was asleep on their sleepist minutes.
getSleepiestMinutesCount :: MinuteMap -> (Guard, [Minute]) -> Int
getSleepiestMinutesCount sleepiestMinutes (guard, minutes) =
    length . filter (== sleepiestMinutes ! guard) $ minutes
