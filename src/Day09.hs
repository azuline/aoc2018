import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Deque.Strict as Deque
import qualified System.IO as IO
import Text.Regex.PCRE

type Player = Int

type Circle = Deque.Deque Int

type Players = Map.Map Player Int -- Map of player ID to score.

main :: IO ()
main = do
  contents <- IO.readFile "../inputs/day09.txt"
  let (playerCount, marbleCount) = parseInput contents

  putStrLn $ "Part 1: " ++ show (part1 playerCount marbleCount)
  putStrLn $ "Part 2: " ++ show (part2 playerCount marbleCount)

-- Parse an int from a string; strip the leading "+" if it exists.
parseInput :: String -> (Int, Int)
parseInput input = case input =~ "\\d+" of
  [[playerCount], [marbleCount]] -> (read playerCount, read marbleCount)
  _ -> error "invalid input"

-- Play normal marble game.
part1 :: Int -> Int -> Int
part1 = findWinningScore

-- Play marble game with x100 last number.
part2 :: Int -> Int -> Int
part2 playerCount marbleCount = findWinningScore playerCount (marbleCount * 100)

-- Find the winning score of the marble game.
-- Set up the initial values and begin emulating the game.
findWinningScore :: Int -> Int -> Int
findWinningScore playerCount marbleCount = emulateGame circle players 1 playerCount marbleCount
  where
    circle = Deque.fromConsAndSnocLists [0] []
    players = Map.fromList [(id, 0) | id <- [1 .. playerCount]]

-- Recursively emulate rounds of the game until we place all the marbles.
emulateGame :: Circle -> Players -> Int -> Int -> Int -> Int
emulateGame circle players count playerCount marbleCount
  | count == marbleCount = getMaxScore players
  | otherwise = emulateGame newCircle newPlayers (count + 1) playerCount marbleCount
  where
    (newCircle, newPlayers) = emulateRound circle players count playerCount

-- Emulate a round of the game. We maintain the invariant that the "current"
-- marble is at the end of the deque.
emulateRound :: Circle -> Players -> Int -> Int -> (Circle, Players)
emulateRound circle players count playerCount
  | count `mod` 23 /= 0 = (Deque.snoc count $ rotate 1 circle, players)
  | otherwise = case Deque.unsnoc $ rotate (-7) circle of
    Just (deleted, newCircle) ->
      let player = (count `mod` playerCount) + 1
          newPlayers = updatePlayers (count + deleted) player players
       in (rotate 1 newCircle, newPlayers)
    Nothing ->
      error "impossible case"

-- Rotate a circle `times` times. Positive times are a right shift; negative
-- are a left shift.
rotate :: Int -> Circle -> Circle
rotate times circle
  | times == 0 = circle
  | times < 0 = rotate (times + 1) $ Deque.shiftRight circle
  | times > 0 = rotate (times - 1) $ Deque.shiftLeft circle

-- Update the players map by adding the score to the player's score.
updatePlayers :: Int -> Player -> Players -> Players
updatePlayers add = Map.adjust (+ add)

-- Get the highest score of a player.
getMaxScore :: Players -> Int
getMaxScore = Map.foldl' max 0
