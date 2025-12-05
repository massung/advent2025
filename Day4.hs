module Days.Day4 (Day4(..)) where

import Data.List as L
import Data.Map.Strict as M
import Days.Solver

data Day4 = Day4

instance Solver Day4 where
  day Day4 = 4

  -- solve each part
  part1 Day4 = toInteger . M.size . accessible . parseMap
  part2 Day4 = toInteger . rmAccessible . parseMap

parseMap :: [String] -> Map (Int, Int) Int
parseMap = M.fromList . L.concatMap row . zip [0..]
  where
    row (y, s) = [((x, y), 0) | (c, x) <- zip s [0..], c == '@']

cardinalTiles :: (Int, Int) -> [(Int, Int)]
cardinalTiles (x, y) = [(x + dx, y + dy) | (dx, dy) <- dirs]
  where
    dirs = [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]

mapAdjacency :: Map (Int, Int) Int -> Map (Int, Int) Int
mapAdjacency m = L.foldr tally m $ M.keys m
  where
    tally xy m' = L.foldr (M.adjust (+1)) m' $ cardinalTiles xy

accessible :: Map (Int, Int) Int -> Map (Int, Int) Int
accessible = M.filter (< 4) . mapAdjacency

rmAccessible :: Map (Int, Int) Int -> Int
rmAccessible m = M.size m - M.size (rm m)
  where
    rm m' = let a = accessible m' in if M.null a then m' else rm $ M.difference m' a
