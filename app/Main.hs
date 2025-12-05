{-# LANGUAGE GADTs #-}

module Main (main) where

import Prelude hiding (Day)
import Days.Day1
import Days.Day2
import Days.Day3
import Days.Day4
import Days.Day5
import Days.Day6
import Days.Solver

data Solveable where
  Solveable :: (Solver a, Show a) => a -> Solveable

days :: [Solveable]
days =
  [ Solveable Day1,
    Solveable Day2,
    Solveable Day3,
    Solveable Day4,
    Solveable Day5,
    Solveable Day6
  ]

main :: IO ()
main = forM_ days solveDay
  where
    solveDay (Solveable solver) = do
      print solver
      solve solver part1 Real >>= putStrLn . (" * part 1 = " ++) . show
      solve solver part2 Real >>= putStrLn . (" * part 2 = " ++) . show
