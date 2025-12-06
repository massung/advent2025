module Days.Day6 (Day6(..)) where

import Days.Solver

data Day6 = Day6 deriving(Show)

data Problem = Problem String [Integer] deriving(Show)

instance Solver Day6 where
  day Day6 = 6

  -- solve each part
  part1 Day6 = sum . map evalProblem . parseProblems readLTR
  part2 Day6 = sum . map evalProblem . parseProblems readTTB

parseProblems :: (String -> [String] -> [[Integer]]) -> [String] -> [Problem]
parseProblems readFn puzzle = parse $ fromJust $ unsnoc puzzle
  where
    parse (ns, ops) = zipWith Problem (words ops) (readFn ops ns)

evalProblem :: Problem -> Integer
evalProblem (Problem op xs) = (if op == "+" then sum else product) xs

readLTR :: String -> [String] -> [[Integer]]
readLTR _ = transpose . map (map read . words)

readTTB :: String -> [String] -> [[Integer]]
readTTB ops = map readCols . transpose . map (splitCols $ findIndices (/= ' ') ops)
  where
    splitCols (x:y:xs) s = drop x (take (y - 1) s):splitCols (y:xs) s
    splitCols [x] s = [drop x s]
    splitCols [] _ = []

readCols :: [String] -> [Integer]
readCols ns =
  let len = maximum (map length ns)
   in map read $ transpose $ map (take len . (++ repeat '0')) ns
