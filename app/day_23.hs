import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List (sort)
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Regex.TDFA

f_name :: FilePath
f_name = "./inputs/day_23/input.txt"

splitChars :: String -> [String]
splitChars x = getAllTextMatches (x =~ "[a-z]+") :: [String]

unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x : xs) = x : unique (filter (/= x) xs)

startsWithT :: String -> Bool
startsWithT x = take 1 x == "t"

getDataFromFile :: FilePath -> IO ([String], HashMap String [String])
getDataFromFile fileName = do
  contents <- readFile fileName
  let fileLines = lines contents
  let links = map splitChars fileLines
  let keysT = unique $ filter startsWithT $ concat links
  let hashMap =
        foldl
          (\acc [k, v] -> HashMap.insertWith (++) k [v] acc)
          HashMap.empty
          (links ++ (map reverse links))
  return (keysT, hashMap)

-- Part 1

subArr :: (Eq a, Ord a) => [a] -> [[a]]
subArr [] = []
subArr xs = unique [sort [x, y] | x <- xs, y <- xs, x /= y]

find_s :: String -> HashMap String [String] -> [[String]]
find_s key hashMap = map (\a -> sort ([key] ++ a)) (filter (\[x, y] -> x `elem` (hashMap HashMap.! y)) subs)
  where
    subs :: [[String]]
    subs = subArr (hashMap HashMap.! key)

part1 = do
  (keysT, hashMap) <- getDataFromFile f_name

  return $ length $ unique $ concatMap (\k -> find_s k hashMap) keysT

-- Part 2
