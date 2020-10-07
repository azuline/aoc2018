import qualified Data.List       as List
import qualified Data.Map.Strict as Map
import qualified System.IO       as IO
import           Text.Regex.PCRE

type Coord = (Int, Int)
-- A map of coordinates to the number of claims on that coordinate.
type CoordCounter = Map.Map Coord Int
-- A tuple representing a claim: (ID, X, Y, width, height).
type Claim = (Int, Int, Int, Int, Int)

main :: IO ()
main = do
    contents <- IO.readFile "../inputs/day03.txt"
    let claims = [ parseClaim line | line <- lines contents ]

    putStrLn $ "Part 1: " ++ show (part1 claims)
    putStrLn $ "Part 2: " ++ show (part2 claims)

-- Parse claim tuple out from a line of the file contents.
parseClaim :: String -> Claim
parseClaim line = (read id, read x, read y, read width, read height)
    where [[id], [x], [y], [width], [height]] = line =~ "\\d+"

-- Return the number of coordinates that have two or more claims on them.
part1 :: [Claim] -> Int
part1 = countCoords . makeCounter

-- Make a CoordCounter from a list of claims.
makeCounter :: [Claim] -> CoordCounter
makeCounter = List.foldl' addClaimToCounter Map.empty

-- For each coordinate that a claim covers, increment the associated coordinate
-- in the coord counter.
addClaimToCounter :: CoordCounter -> Claim -> CoordCounter
addClaimToCounter counter =
    List.foldl' (\map key -> Map.insertWith (+) key 1 map) counter . getAllCoords

-- Return all coordinates claimed by a claim.
getAllCoords :: Claim -> [Coord]
getAllCoords (_, x, y, width, height) = [ (x, y) | x <- [x..(x + width - 1)],
                                                   y <- [y..(y + height - 1)] ]

-- Return the number of coordinates with more than one claim.
countCoords :: CoordCounter -> Int
countCoords = length . filter (> 1) . Map.elems

-- Return the ID of the intact claim.
part2 :: [Claim] -> Int
part2 claims = fst5 $ List.find (isIntactClaim counter) claims
    where counter = makeCounter claims
          fst5 (Just (x, _, _, _, _)) = x

-- Return whether a claim is intact.
isIntactClaim :: CoordCounter -> Claim -> Bool
isIntactClaim counter = all (\c -> Map.lookup c counter == Just 1) . getAllCoords
