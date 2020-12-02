import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified System.IO as IO

-- Part 1 types.
type Coord = (Int, Int)

type SerialNumber = Int

type PowerLevel = Int

type PowerMap = Map.Map Coord PowerLevel

type SumPowerLevel = Int

-- Part 2 types.
type Square = (Int, Int, Int)

type SummedAreaTable = Map.Map Coord SumPowerLevel

main :: IO ()
main = do
  contents <- IO.readFile "../inputs/day11.txt"

  let serialNumber = read contents
      powerMap = constructPowerMap serialNumber

  putStrLn $ "Part 1: " ++ show (part1 powerMap)
  putStrLn $ "Part 2: " ++ show (part2 powerMap)

-- Construct a PowerMap: a map of fuel cell coordinates to their power levels.
constructPowerMap :: SerialNumber -> PowerMap
constructPowerMap serialNumber = Map.fromList list
  where
    list = [((x, y), calcPowerLevel (x, y) serialNumber) | x <- [1 .. 300], y <- [1 .. 300]]

-- Calculate the power level of a fuel cell given the coordinate and serial number.
calcPowerLevel :: Coord -> SerialNumber -> PowerLevel
calcPowerLevel (x, y) serialNumber = getHundredsDigit (((rackId * y) + serialNumber) * rackId) - 5
  where
    rackId = x + 10

-- Return the hundreds digit of an integer.
getHundredsDigit :: Int -> Int
getHundredsDigit int = (int `mod` 1000) `div` 100

-- Return the top-left coordinate of the most powerful 3x3 square.
part1 :: PowerMap -> Coord
part1 powerMap = maximumBy (get3x3SquarePowerLevel powerMap) coords
  where
    coords = [(x, y) | x <- [1 .. 298], y <- [1 .. 298]]

-- Get the power level of a 3x3 square given its top left coordinate.
get3x3SquarePowerLevel :: PowerMap -> Coord -> SumPowerLevel
get3x3SquarePowerLevel powerMap (x, y) = sum powers
  where
    powers = [powerMap Map.! (x', y') | x' <- [x .. x + 2], y' <- [y .. y + 2]]

-- Return the maximum of a list based on the result of the passed-in evaluation function.
maximumBy :: (a -> Int) -> [a] -> a
maximumBy calcF (head : tail) = fst $ List.foldl' foldF (head, calcF head) tail
  where
    foldF (elem, val) elem' = if val' > val then (elem', val') else (elem, val)
      where
        val' = calcF elem'

-- Return the (x, y, size) identifier of the most powerful square.
part2 :: PowerMap -> Square
part2 powerMap = maximumBy (getArbSquarePowerLevel sat) squares
  where
    squares = [(x, y, s) | x <- [1 .. 300], y <- [1 .. 300], s <- [1 .. 301 - max x y]]
    sat = constructSummedAreaTable powerMap

-- https://en.wikipedia.org/wiki/Summed-area_table
constructSummedAreaTable :: PowerMap -> SummedAreaTable
constructSummedAreaTable powerMap = List.foldl' foldF Map.empty coords
  where
    coords = [(x, y) | y <- [1 .. 300], x <- [1 .. 300]]
    foldF sat (x, y) = Map.insert (x, y) sum' sat
      where
        sum' = coordPowerLevel + xLessBlock + yLessBlock - xyLessBlock
        coordPowerLevel = powerMap Map.! (x, y)
        xLessBlock = if y == 1 then 0 else sat Map.! (x, y - 1)
        yLessBlock = if x == 1 then 0 else sat Map.! (x - 1, y)
        xyLessBlock = if x == 1 || y == 1 then 0 else sat Map.! (x - 1, y - 1)

-- Get the power level of a square by calculating:
--   T(x - 1, y - 1) - T(x + s - 1, y - 1) - T(x - 1, y + s - 1) + T(x + s - 1, y + s - 1)
getArbSquarePowerLevel :: SummedAreaTable -> Square -> PowerLevel
getArbSquarePowerLevel summedAreas (x, y, size) = wholeBlock - xLessBlock - yLessBlock + xyLessBlock
  where
    wholeBlock = if x == 1 || y == 1 then 0 else summedAreas Map.! (x - 1, y - 1)
    xLessBlock = if y == 1 then 0 else summedAreas Map.! (x + size - 1, y - 1)
    yLessBlock = if x == 1 then 0 else summedAreas Map.! (x - 1, y + size - 1)
    xyLessBlock = summedAreas Map.! (x + size - 1, y + size - 1)
