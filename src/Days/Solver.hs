{-# LANGUAGE OverloadedStrings #-}

module Days.Solver
 ( Mode(..)
 , Solver(..)
 )
where

import Data.String
import Prelude hiding (Day)

data Mode
  = Test
  | Real

rootPath :: FilePath
rootPath = "/Users/jeff/Developer/Advent/advent2025"

class Solver a where
  day :: a -> Int

  -- reads the input test or real data for this day
  puzzleData :: a -> Mode -> IO [String]
  puzzleData solver mode = lines <$> readFile (path mode)
    where
      path Test = printf "%s/data/day%d/test.txt" rootPath (day solver)
      path Real = printf "%s/data/day%d/real.txt" rootPath (day solver)

  -- solves supplied input lines
  part1 :: a -> [String] -> Integer
  part2 :: a -> [String] -> Integer

  -- load the puzzle data and run a puzzle part
  solve :: a -> (a -> [String] -> Integer) -> Mode -> IO Integer
  solve solver part mode = part solver <$> puzzleData solver mode
