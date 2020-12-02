import qualified Data.List as List
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified System.IO as IO

type Coord = (Int, Int)

type Paths = Map.Map Coord Char

type Turns = Int

type Cart = (Coord, Char, Turns)

type Carts = [Cart]

main :: IO ()
main = do
  contents <- IO.readFile "../inputs/day13.txt"
  let (paths, carts) = parseInput contents

  putStrLn $ "Part 1: " ++ show (part1 paths carts)
  putStrLn $ "Part 2: " ++ show (part2 paths carts)

-- Parse the paths and initial carts from the input string.
parseInput :: String -> (Paths, Carts)
parseInput input = List.foldl' addRowToMap (Map.empty, []) lines'
  where
    lines' = zip [0 ..] $ lines input

cartChars = Map.fromList [('^', '|'), ('>', '-'), ('v', '|'), ('<', '-')]

-- Parse a row of the input to the carts and maps.
addRowToMap :: (Paths, Carts) -> (Int, String) -> (Paths, Carts)
addRowToMap acc (y, row) = List.foldl' foldF acc $ zip [0 ..] row
  where
    foldF (paths, carts) (x, char) = case Map.lookup char cartChars of
      Nothing -> (Map.insert (x, y) char paths, carts)
      Just char' -> (Map.insert (x, y) char' paths, ((x, y), char, 0) : carts)

-- Return the coordinate of the first crashed cart.
part1 :: Paths -> Carts -> Coord
part1 paths carts = getFirstCrashedCart paths carts []

-- Get the first crashed cart. Move cart by cart from the unmoved list to the
-- moved list, and after each movement, check for a crash; after all carts have
-- been moved, repeat.
getFirstCrashedCart :: Paths -> Carts -> Carts -> Coord
getFirstCrashedCart paths moved [] = getFirstCrashedCart paths [] $ List.sort moved
getFirstCrashedCart paths moved (cart : unmoved) =
  case isCrashedCart movedCart (moved ++ unmoved) of
    Just _ -> let (coord, _, _) = movedCart in coord
    Nothing -> getFirstCrashedCart paths (movedCart : moved) unmoved
  where
    movedCart = moveCart paths cart

-- Move a cart one unit along the paths.
moveCart :: Paths -> Cart -> Cart
moveCart paths (coord, char, turns) = (newCoord, newChar, newTurns)
  where
    newCoord = getNextCoord coord char
    pathChar = paths ! newCoord
    newChar = getNextChar char pathChar turns
    newTurns = if pathChar == '+' then (turns + 1) `mod` 3 else turns

-- Get the next coord given the direction of the cart.
getNextCoord :: Coord -> Char -> Coord
getNextCoord (x, y) '^' = (x, y - 1)
getNextCoord (x, y) '>' = (x + 1, y)
getNextCoord (x, y) 'v' = (x, y + 1)
getNextCoord (x, y) '<' = (x - 1, y)

-- Get the new cart character (i.e. direction) from the old direction, the path
-- char, and the number of turns.
getNextChar :: Char -> Char -> Int -> Char
getNextChar char '-' _ = char
getNextChar char '|' _ = char
getNextChar '^' '\\' _ = '<'
getNextChar '^' '/' _ = '>'
getNextChar '>' '\\' _ = 'v'
getNextChar '>' '/' _ = '^'
getNextChar 'v' '\\' _ = '>'
getNextChar 'v' '/' _ = '<'
getNextChar '<' '\\' _ = '^'
getNextChar '<' '/' _ = 'v'
getNextChar char '+' turns = intToDirection ! directionInt
  where
    directionInt = ((directionToInt ! char) + turns + 3) `mod` 4

-- Two parallel maps for calculating the new direction of the cart on an intersection.
directionToInt = Map.fromList $ zip "^>v<" [0 ..]

intToDirection = Map.fromList $ zip [0 ..] "^>v<"

-- Given a cart, see if it has crashed into any of the other carts.
isCrashedCart :: Cart -> Carts -> Maybe Cart
isCrashedCart (coord, _, _) = List.find (\(coord', _, _) -> coord' == coord)

-- Return the coordinate of the first crashed cart.
part2 :: Paths -> Carts -> Coord
part2 paths carts = getLastCart paths carts []

-- Get the last uncrashed cart. Move cart by cart from the unmoved list to the
-- moved list, and after each movement, check for a crash; after all carts have
-- been moved, repeat.
getLastCart :: Paths -> Carts -> Carts -> Coord
getLastCart paths moved [] = getLastCart paths [] $ List.sort moved
getLastCart paths [] [(coord, _, _)] = coord
getLastCart paths moved (cart : unmoved) =
  case isCrashedCart movedCart (moved ++ unmoved) of
    Nothing -> getLastCart paths (movedCart : moved) unmoved
    Just crashedCart ->
      let newMoved = List.delete crashedCart moved
          newUnmoved = List.delete crashedCart unmoved
       in getLastCart paths newMoved newUnmoved
  where
    movedCart = moveCart paths cart
