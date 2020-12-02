import qualified Data.Char as Char
import Data.Foldable (concatMap)
import qualified Data.List as List
import Data.Sequence (Seq, (!?), (><))
import qualified Data.Sequence as Seq
import qualified System.IO as IO

type NumRecipes = Int

type Recipes = Seq Int

type Index = Int

type Elves = (Index, Index)

main :: IO ()
main = do
  contents <- IO.readFile "../inputs/day14.txt"
  let intInput = read contents
      seqInput = Seq.fromList . List.map Char.digitToInt . stripNewline $ contents

  putStrLn $ "Part 1: " ++ part1 intInput
  putStrLn $ "Part 2: " ++ show (part2 seqInput)

-- Remove the trailing newline from a string.
stripNewline :: String -> String
stripNewline string = if List.last string == '\n' then init string else string

-- A constant sequence of the initial two recipes.
initialRecipes = Seq.fromList [3, 7]

-- Get the first 10 recipes after `input` recipes as a string.
part1 :: NumRecipes -> String
part1 numRecipes = renderRecipes . Seq.take 10 . Seq.drop numRecipes $ recipes
  where
    recipes = makeNumRecipes initialRecipes (numRecipes + 10) (0, 1)

-- Convert a sequence of ints int a string of digits.
renderRecipes :: Recipes -> String
renderRecipes = concatMap show

-- Generate recipes until there are at least `num` recipes.
makeNumRecipes :: Recipes -> NumRecipes -> Elves -> Recipes
makeNumRecipes recipes num elves
  | Seq.length recipes >= num = recipes
  | otherwise = makeNumRecipes newRecipes num newElves
  where
    (newRecipes, newElves) = makeNewRecipes recipes elves

-- Make one set of new recipes from the given recipes and elves.
makeNewRecipes :: Recipes -> Elves -> (Recipes, Elves)
makeNewRecipes recipes (elf1, elf2) = (newRecipes, (newElf1, newElf2))
  where
    Just recipe1 = recipes !? elf1
    Just recipe2 = recipes !? elf2
    newRecipes = recipes >< digits (recipe1 + recipe2)
    newElf1 = (1 + elf1 + recipe1) `mod` Seq.length newRecipes
    newElf2 = (1 + elf2 + recipe2) `mod` Seq.length newRecipes

-- Convert a 1/2 digit number into a sequence of their digits.
digits :: Int -> Seq Int
digits int
  | int < 10 = Seq.singleton int
  | otherwise = Seq.fromList [div', mod']
  where
    (div', mod') = divMod int 10

-- Get the number of recipes that appear before the input `subseq`.
part2 :: Recipes -> NumRecipes
part2 subseq = findRecipeSubstring initialRecipes subseq (0, 1)

findRecipeSubstring :: Recipes -> Recipes -> Elves -> NumRecipes
findRecipeSubstring recipes subseq elves = case lookForSubseq recipes subseq of
  Just numRecipes -> numRecipes
  Nothing -> findRecipeSubstring newRecipes subseq newElves
  where
    (newRecipes, newElves) = makeNewRecipes recipes elves

lookForSubseq :: Recipes -> Recipes -> Maybe NumRecipes
lookForSubseq recipes subseq
  | subseq == rightmost = Just numToDrop
  | subseq == secondRightmost = Just (numToDrop - 1)
  | otherwise = Nothing
  where
    rightmost = Seq.drop numToDrop recipes
    secondRightmost = Seq.take (Seq.length subseq) . Seq.drop (numToDrop - 1) $ recipes
    numToDrop = Seq.length recipes - Seq.length subseq
