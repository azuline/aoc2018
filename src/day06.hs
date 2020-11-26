import qualified Data.List as List
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified System.IO as IO

type Coord = (Int, Int)

type Box = Map.Map Coord (Maybe Coord)

main :: IO ()
main = do
  contents <- IO.readFile "../inputs/day06.txt"
  let coords = [lineToCoord line | line <- lines contents]

  putStrLn $ "Part 1: " ++ show (part1 coords)
  putStrLn $ "Part 2: " ++ show (part2 coords)

-- Convert a line to a tuple of (x, y) coordinates.
lineToCoord :: String -> Coord
lineToCoord line = (read x :: Int, read y :: Int)
  where
    (x, y) = case splitOn ", " line of
      [x, y] -> (x, y)
      _ -> error "Impossible case."

-- Approach: We first find the largest X and Y coordinates, and create a 2D
-- "box" of locations of X width and Y height. Then, for each location in the
-- box, we calculate the coordinate it is closest to. We construct a set of
-- coordinates touching the perimeter of the box; these are the "infinite"
-- coordinates. We then tabulate the number of locations corresponding to each
-- finite coordinate and find the coordinate with the most locations.
part1 :: [Coord] -> Int
part1 coords = snd $ findLargestFiniteCoord finiteCoords box
  where
    dimensions = findDimensions coords
    box = makeClosestBox coords dimensions
    infiniteCoords = findInfiniteCoords dimensions box
    finiteCoords = List.filter (\c -> not $ Set.member c infiniteCoords) coords

-- Return the largest X and largest Y coords. Technically width + 1 and height
-- + 1 but who cares?
findDimensions :: [Coord] -> Coord
findDimensions = List.foldl1' foldF
  where
    foldF (accX, accY) (x, y) = (max accX x, max accY y)

-- Construct a map of locations in the box to the coordinate they are closest
-- to.
makeClosestBox :: [Coord] -> Coord -> Box
makeClosestBox coords (width, height) = List.foldl' foldF Map.empty emptyBox
  where
    emptyBox = makeEmptyBox width height
    foldF map loc = Map.insert loc closestCoord map
      where
        closestCoord = calcClosestCoord coords loc

-- Make an empty box of width + 1 and height + 1 dimensions.
makeEmptyBox :: Int -> Int -> [Coord]
makeEmptyBox width height = [(x, y) | x <- [0 .. width], y <- [0 .. height]]

-- Return the coordinate closest to the location. If two coordinates are
-- equidistant from the location, return Nothing
calcClosestCoord :: [Coord] -> Coord -> Maybe Coord
calcClosestCoord (coord : coords) loc = fst $ List.foldl' foldF initialVal coords
  where
    initialVal = (Just coord, manhattanDistance coord loc)
    foldF (accCoord, accDist) coord'
      | accDist == dist = (Nothing, accDist)
      | accDist < dist = (accCoord, accDist)
      | otherwise = (Just coord', dist)
      where
        dist = manhattanDistance coord' loc

-- Calculate the manhattan distance between two coordinates.
manhattanDistance :: Coord -> Coord -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

-- Find all coordinates along the perimeter of the box. These are the infinite
-- cordinates.
findInfiniteCoords :: Coord -> Box -> Set.Set Coord
findInfiniteCoords (width, height) box = List.foldl' foldF Set.empty perimeter
  where
    foldF acc (Just coord) = Set.insert coord acc
    foldF acc Nothing = acc
    perimeter =
      [ box Map.! (x, y) | x <- [0 .. width], y <- [0 .. height], x == 0 || x == width || y == 0 || y == height
      ]

-- Find the largest finite coordinate and return it alongside the number of
-- locations closest to it.
findLargestFiniteCoord :: [Coord] -> Box -> (Coord, Int)
findLargestFiniteCoord coords box = List.foldl' foldF ((0, 0), 0) coords
  where
    foldF (accCoord, accNumLocs) coord =
      if numLocs > accNumLocs then (coord, numLocs) else (accCoord, accNumLocs)
      where
        numLocs = length $ Map.filter (== Just coord) box

part2 :: [Coord] -> Int
part2 coords = length $ List.filter (\(_, dist) -> dist < 10000) locsWithDistance
  where
    (width, height) = findDimensions coords
    emptyBox = makeEmptyBox width height
    locsWithDistance = List.map (withTotalDistance coords) emptyBox

withTotalDistance :: [Coord] -> Coord -> (Coord, Int)
withTotalDistance coords location = (location, totalDistance)
  where
    totalDistance = sum $ List.map (manhattanDistance location) coords
