{-# LANGUAGE OverloadedStrings #-}

module Days.Utils
 ( Day(..)
 , Mode(..)
 , rootPath
 , dataPath
 , readLines
 )
where

import Data.String
import Prelude hiding (Day)

data Mode
  = Test
  | Real

newtype Day = Day Int

rootPath :: FilePath
rootPath = "/Users/jeff/Developer/Advent/advent2025"

dataPath :: Day -> Mode -> FilePath
dataPath (Day day) Test = printf "%s/data/day%d/test.txt" rootPath day
dataPath (Day day) Real = printf "%s/data/day%d/real.txt" rootPath day

readLines :: Day -> Mode -> IO [String]
readLines day mode = contents <&> lines
  where
    contents :: IO String
    contents = readFile $ dataPath day mode
