import Text.Regex.TDFA

f_name :: FilePath
f_name = "./inputs/day_14/input.txt"

splitInt :: String -> [String]
splitInt x = getAllTextMatches (x =~ "-?[0-9]+") :: [String]

parseLine :: String -> (Int, Int, Int, Int)
parseLine line = (read a, read b, read c, read d) where [a, b, c, d] = splitInt line

getDataFromFile :: FilePath -> IO [(Int, Int, Int, Int)]
getDataFromFile fileName = do
  contents <- readFile fileName
  let fileLines = lines contents
  return $ map parseLine fileLines

-- Part 1
simulate_robot :: (Int, Int, Int, Int) -> Int -> Int -> Int -> (Int, Int)
simulate_robot (x, y, dx, dy) seconds width height = (x'', y'')
  where
    x' = x + dx * seconds
    y' = y + dy * seconds
    dx' = dx
    dy' = dy
    x'' = x' `mod` width
    y'' = y' `mod` height

part1 = do
  robots <- getDataFromFile f_name
  let width = 101
  let height = 103
  let last_pos = map (\r -> simulate_robot r 100 width height) robots
  let robots_in_sector_A = length $ filter (\(x, y) -> x < width `div` 2 && y < height `div` 2) last_pos
  let robots_in_sector_B = length $ filter (\(x, y) -> x > width `div` 2 && y < height `div` 2) last_pos
  let robots_in_sector_C = length $ filter (\(x, y) -> x < width `div` 2 && y > height `div` 2) last_pos
  let robots_in_sector_D = length $ filter (\(x, y) -> x > width `div` 2 && y > height `div` 2) last_pos
  return $ product [robots_in_sector_A, robots_in_sector_B, robots_in_sector_C, robots_in_sector_D]
