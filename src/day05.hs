import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Ord  as Ord
import qualified System.IO as IO

main :: IO ()
main = do
    contents <- IO.readFile "../inputs/day05.txt"
    let polymer = init contents -- Strip trailing \n.

    putStrLn $ "Part 1: " ++ show (part1 polymer)
    putStrLn $ "Part 1: " ++ show (part2 polymer)

-- Return the length of the reacted polymer.
part1 :: String -> Int
part1 = length . reactPolymer

-- Fully react the polymer. We re-construct the polymer character by character and
-- handle all potential reactions.
reactPolymer :: String -> String
reactPolymer = List.foldl' f ""
    where f []             elem = [elem]
          f (prev:reacted) elem = if willReact prev elem
                                  then reacted
                                  else elem:prev:reacted

-- Returns whether or not two characters will react with each other.
willReact :: Char -> Char -> Bool
willReact c1 c2 = lowerUpper || upperLower
    where lowerUpper = Char.isLower c1 && Char.toUpper c1 == c2
          upperLower = Char.isUpper c1 && Char.toLower c1 == c2

-- Find the smallest length of a polymer with one unit type removed.
part2 :: String -> Int
part2 polymer = List.minimum
                . map (\c -> part1 . filter (\c' -> c /= Char.toLower c') $ polymer)
                $ ['a'..'z']
