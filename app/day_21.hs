import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

f_name :: FilePath
f_name = "./inputs/day_21/input_test.txt"

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just x) = x
fromMaybe d Nothing = d

getDataFromFile :: FilePath -> IO [Int]
getDataFromFile fileName = do
  contents <- readFile fileName
  let fileLines = lines contents

  return $ map read fileLines

-- Part 1

num_pad_locations:: HashMap Int (Int, Int)
num_pad_locations = HashMap.fromList [
  (7, (0, 0)),
  (8, (1, 0)),
  (9, (2, 0)),
  (4, (0, 1)),
  (5, (1, 1)),
  (6, (2, 1)),
  (1, (0, 2)),
  (2, (1, 2)),
  (3, (2, 2)),
  (0, (1, 3)),
  (-1, (2, 3))
]

get_num_pad_location :: Int -> (Int, Int)
get_num_pad_location num = fromMaybe (0, 3) $ HashMap.lookup num num_pad_locations


get


part1 = do
  nums <- getDataFromFile f_name

  return 1
