module Days.Day2 (Day2(..)) where

import Data.List.Split (splitWhen)
import Data.List.Extra
import Days.Solver

data Day2 = Day2 deriving(Show)

instance Solver Day2 where
  day Day2 = 2

  -- solve each part
  part1 Day2 = sum . concatMap simpleInvalidIDs . parseRanges . concat
  part2 Day2 = sum . concatMap invalidIDs . parseRanges . concat

parseRanges :: String -> [[Integer]]
parseRanges line = map parseRange $ splitWhen (== ',') line
  where
    parseRange :: String -> [Integer]
    parseRange s = let (start, end) = break (== '-') s in
      enumFromTo (read start) (read $ drop 1 end)

simpleInvalidIDs :: [Integer] -> [Integer]
simpleInvalidIDs = filter (isInvalid . show)
  where
    isInvalid :: String -> Bool
    isInvalid s = uncurry (==) $ splitAt (length s `div` 2) s

invalidIDs :: [Integer] -> [Integer]
invalidIDs = filter (repeats 1 . show)
 where
   repeats :: Int -> String -> Bool
   repeats len s = case chunksOf len s of
     xs@(_:_:_) -> allSame xs || repeats (len + 1) s
     _ -> False
