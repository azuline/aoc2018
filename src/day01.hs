import Data.Set (Set)
import qualified Data.Set as Set
import qualified System.IO as IO

type Sum = Int

type Sums = [Sum]

type Seen = Set Int

main :: IO ()
main = do
  contents <- IO.readFile "../inputs/day01.txt"
  let integers = [parseInt word | word <- lines contents]

  putStrLn $ "Part 1: " ++ show (part1 integers)
  putStrLn $ "Part 2: " ++ show (part2 integers)

-- Parse an int from a string; strip the leading "+" if it exists.
parseInt :: String -> Int
parseInt ('+' : int) = read int
parseInt int = read int

-- Return the sum of the integers.
part1 :: [Int] -> Sum
part1 = sum

-- Find the first repeated sum in the rolling sum of the integers.
part2 :: [Int] -> Sum
part2 integers = findFirstRepeatSum rollingSum Set.empty
  where
    rollingSum = scanl1 (+) $ cycle integers

-- Find first repeated element in a list of sums.
findFirstRepeatSum :: Sums -> Seen -> Sum
findFirstRepeatSum (x : xs) seen
  | Set.member x seen = x
  | otherwise = findFirstRepeatSum xs (Set.insert x seen)
