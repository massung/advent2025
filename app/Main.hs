{-# LANGUAGE GADTs #-}

module Main (main) where

import Prelude hiding (Day)
import Days.Day1
import Days.Day2
import Days.Day3
import Days.Day4
import Days.Day5
import Days.Solver

data Solveable where
  Solveable :: Solver a => a -> Solveable

days :: [Solveable]
days =
  [ Solveable Day1,
    Solveable Day2,
    Solveable Day3,
    Solveable Day4,
    Solveable Day5
  ]

_allSolutions :: IO ()
_allSolutions = forM_ days solveDay
  where
    solveDay (Solveable solver) = do
      solve solver part1 Real >>= print
      solve solver part2 Real >>= print

main :: IO ()
main = do
  solve Day4 part1 Real >>= print
  solve Day4 part2 Real >>= print
