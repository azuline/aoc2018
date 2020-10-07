{-# LANGUAGE TupleSections #-}

import qualified Data.List       as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Ord        as Ord
import qualified System.IO       as IO

type Words     = [String]
type CharIndex = (Int, Char)
type SeenMap   = Map CharIndex [String]

main :: IO ()
main = do
    contents <- IO.readFile "../inputs/day02.txt"
    let words = List.words contents

    putStrLn $ "Part 1: " ++ show (part1 words)
    putStrLn $ "Part 2: " ++ part2 words

-- Return number of words with exactly `n` of any letter for `n \in {2, 3]`.
part1 :: Words -> Int
part1 words = countMatchingWords 2 words * countMatchingWords 3 words

-- Return the number of words that have `n` of any letter.
countMatchingWords :: Int -> Words -> Int
countMatchingWords n = length . filter (hasNOfAnyLetter n)

-- Return whether `string` has exactly `n` occurrences of any character.
hasNOfAnyLetter :: Int -> String -> Bool
hasNOfAnyLetter n = elem n . Map.elems . buildFreqMap

-- Build a frequency map from a list of elements.
buildFreqMap :: (Ord a) => [a] -> Map a Int
buildFreqMap = Map.fromListWith (+) . map (,1)

-- Return the common letters of two neighboring words.
part2 :: [String] -> String
part2 = removeDifferingCharacter . findNeighboringWords

-- Find the two neighboring words (i.e. words with only one differing char/index pair).
findNeighboringWords :: Words -> (String, String)
findNeighboringWords (word:words) = go words (insertSeen word Map.empty)
    where charsToMatch            = length word - 1
          go (x:xs) seen = if charsMatched == charsToMatch
                           then (x, matchedWord)
                           else go xs (insertSeen x seen)
              where (matchedWord, charsMatched) = mostMatches x seen

-- Return whether the passed-in word has `charsToMatch` matches in the seen map.
mostMatches :: String -> SeenMap -> (String, Int)
mostMatches word seen = List.maximumBy (Ord.comparing snd)
                        . Map.toList
                        . buildFreqMap
                        . concatMap (\k -> Map.findWithDefault [] k seen)
                        $ zip [0..] word

-- Insert a word into the map of seen words.
insertSeen :: String -> SeenMap -> SeenMap
insertSeen word seen =
    List.foldl' (\map key -> Map.insertWith (++) key [word] map) seen $ zip [0..] word

-- Remove the single different character between two strings.
removeDifferingCharacter :: (String, String) -> String
removeDifferingCharacter (str1, str2) = [ c1 | (c1, c2) <- zip str2 str2, c1 == c2 ]
