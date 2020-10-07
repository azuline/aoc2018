import qualified Data.List   as List
import qualified System.IO   as IO

main :: IO ()
main = do
    contents <- IO.readFile "../inputs/day08.txt"
    let integers = [ read word :: Int | word <- List.words contents ]

    putStrLn $ "Part 1: " ++ show (part1 integers)
    putStrLn $ "Part 2: " ++ show (part2 integers)


part1 :: [Int] -> Int
part1 = fst . sumMetadataEntries

-- Return the sum of all metadata entries.
sumMetadataEntries :: [Int] -> (Int, [Int])
sumMetadataEntries []                      = (0, [])
sumMetadataEntries (childQty:metaQty:ints) = (sum metadataEntries + childSum, remainingInts)
    where (metadataEntries, remainingInts) = List.splitAt metaQty childlessInts
          (childSum, childlessInts)        = sumChildrenMetadata ints childQty

-- Sum the metadata entries for the children; return the sum and the remaining
-- unprocessed integers.
sumChildrenMetadata :: [Int] -> Int -> (Int, [Int])
sumChildrenMetadata ints childQty = List.foldl' foldF (0, ints) $ replicate childQty 0
    where foldF (sum', ints') _ =
            let (retSum, retInts) = sumMetadataEntries ints'
            in  (sum' + retSum, retInts)

part2 :: [Int] -> Int
part2 = fst . getNodeValue

-- Get the value of the current root node in the remaining tree of integers.
getNodeValue :: [Int] -> (Int, [Int])
getNodeValue []                      = (0, [])
getNodeValue (childQty:metaQty:ints)
    | childQty == 0 = (sum metadataEntries, remainingInts)
    | otherwise     = (value, remainingInts)
    where value                            = getChosenChildSum childValues metadataEntries
          (metadataEntries, remainingInts) = List.splitAt metaQty childlessInts
          (childValues, childlessInts)     = gatherChildValues ints childQty

-- Sum the values of the children chosen by the metadata entries.
getChosenChildSum :: [Int] -> [Int] -> Int
getChosenChildSum childValues metadataEntries = sum chosenChildren
    where chosenChildren = [ getOrZero childValues index | index <- metadataEntries ]

-- Get the value from the list at the given index; if the index is out of
-- bounds, return zero. Treat the array as 1-indexed.
getOrZero :: [Int] -> Int -> Int
getOrZero list index
    | index > length list = 0
    | index <= 0          = 0
    | otherwise           = list !! (index - 1)

-- Gather the values of the children; return them alongside the remaining
-- unprocessed integers.
gatherChildValues :: [Int] -> Int -> ([Int], [Int])
gatherChildValues ints childQty = List.foldl' foldF ([], ints) $ replicate childQty 0
    where foldF (values, ints') _ =
              let (retValue, retInts) = getNodeValue ints'
              in (values ++ [retValue], retInts)
