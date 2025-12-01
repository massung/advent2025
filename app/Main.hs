module Main (main) where

import Prelude hiding (Day)
import Days.Day1
import Days.Utils

solvePart :: ([String] -> Integer) -> Day -> Mode -> IO Integer
solvePart part day mode = part <$> readLines day mode

day1 :: IO ()
day1 = do
  solvePart Days.Day1.part1 (Day 1) Test >>= print
  solvePart Days.Day1.part2 (Day 1) Real >>= print

main :: IO ()
main = do
  day1
