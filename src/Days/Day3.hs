module Days.Day3 (Day3(..)) where

import Data.List.Extra
import Days.Solver

data Day3 = Day3 deriving(Show)

instance Solver Day3 where
  day Day3 = 3

  -- solve each part
  part1 Day3 = sum . map (findJoltage 2)
  part2 Day3 = sum . map (findJoltage 12)

findJoltage :: Int -> String -> Integer
findJoltage len = read . digits len
  where
    digits :: Int -> String -> String
    digits 1 s = [maximum s]
    digits n s =
      let d = maximum $ dropEnd (n - 1) s
          i = fromJust $ elemIndex d s
       in d : digits (n - 1) (drop (i + 1) s)
