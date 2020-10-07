import qualified Data.List       as List
import qualified Data.Tuple      as Tuple
import qualified System.IO       as IO
import           Text.Regex.PCRE

-- Consider the "final" message as a set of points that exist in a smallest
-- range of units on the y-axis. We calculate the final message by looking for
-- the number of seconds that are required for all points to exist in this
-- range.

type Velocity = (Int, Int)
type Position = (Int, Int)
type Point = (Position, Velocity)
type Bounds = (Int, Int, Int, Int)

main :: IO ()
main = do
    contents <- IO.readFile "../inputs/day10.txt"
    let points = [ parsePoint word | word <- lines contents ]
        (time, movedPoints) = movePoints points

    putStrLn $ "Part 1:\n" ++ render movedPoints
    putStrLn $ "Part 2: " ++ show time

-- Parse an int from a string; strip the leading "+" if it exists.
parsePoint :: String -> Point
parsePoint input = case input =~ "-?\\d+" of
        [[x], [y], [vx], [vy]] -> ((read x, read y), (read vx, read vy))
        _                      -> error "invalid input"

-- Move the points to their final message state.
movePoints :: [Point] -> (Int, [Point])
movePoints points = movePointsAux points 0 900000 -- High default value.

movePointsAux :: [Point] -> Int -> Int -> (Int, [Point])
movePointsAux points time lastHeight
    | height > lastHeight = (time, points)
    | otherwise           = movePointsAux movedPoints (time + 1) height
    where movedPoints = movePointsOnce points
          height      = getPointsHeight movedPoints

-- Get the height of the bounding box of the points.
getPointsHeight :: [Point] -> Int
getPointsHeight points = topBound - bottomBound
    where (_, _, topBound, bottomBound) = calculateBounds points

-- Calculate the bounds of the points: the max/min x and y.
calculateBounds :: [Point] -> Bounds
calculateBounds (((x, y), _):points) = List.foldl' foldF (x, x, y, y) points
    where foldF (l, r, t, b) ((x, y), _) = (min x l, max x r, max y t, min y b)

-- Emulate one unit of time and transform the position of the vectors by their
-- velocities.
movePointsOnce :: [Point] -> [Point]
movePointsOnce = List.map (\ ((x, y), (vx, vy)) -> ((x + vx, y + vy), (vx, vy)))

-- Render a list of points into a visualization.
-- Sort the coordinates, first by Y and then by X, very crappily by sorting them on `10000y + x`
render :: [Point] -> String
render points = positionsToString positions leftBound
    where positions            = List.nub $ List.sortOn Tuple.swap $ List.map fst points
          (leftBound, _, _, _) = calculateBounds points

-- Convert a list of sorted positions into a visualized string.
-- This is really a monstrosity but whatever.
positionsToString :: [Position] -> Int -> String
positionsToString positions@((_, y):_) leftBound =
    positionsToStringAux positions (leftBound - 1, y) leftBound ""

positionsToStringAux :: [Position] -> Position -> Int -> String -> String
positionsToStringAux []                 _              _         string = string
positionsToStringAux ((x, y):positions) (lastX, lastY) leftBound string
    | y == lastY = positionsToStringAux positions (x, y) leftBound sameLineString
    | otherwise  = positionsToStringAux positions (x, y) leftBound newLineString
    where sameLineString = string ++ [ ' ' | _ <- [lastX..x - 2] ] ++ "#"
          newLineString  = string ++ '\n':[ ' ' | _ <- [leftBound..x - 1] ] ++ "#"
