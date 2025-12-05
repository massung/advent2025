module Days.Day5 (Day5(..)) where

import Days.Solver

data Day5 = Day5 deriving(Show)

instance Solver Day5 where
  day Day5 = 5

  -- solve each part
  part1 Day5 = toInteger . length . freshIds . readIngredients
  part2 Day5 = sum . validFreshIds . fst . readIngredients

readIngredients :: [String] -> ([(Integer, Integer)], [Integer])
readIngredients = parse . break (== "")
  where
    parse (ranges, ids) = (map parseRange ranges, map read $ drop 1 ids)
    parseRange s = let (a, b) = break (== '-') s in (read a, read $ drop 1 b)

freshIds :: ([(Integer, Integer)], [Integer]) -> [Integer]
freshIds (ranges, ids) = filter isFresh ids
  where
    isFresh x = any (\(a, b) -> x >= a && x <= b) ranges

validFreshIds :: [(Integer, Integer)] -> [Integer]
validFreshIds = map (\(a,b) -> b-a+1) . foldl' coalesce [] . sort
  where
    coalesce [] (a, b) = [(a, b)]
    coalesce ((a, b):rs) (c, d)
      | c <= b+1 && d > b = (a, d):rs
      | c <= b+1 = (a, b):rs
      | otherwise = (c, d):(a, b):rs
