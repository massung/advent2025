module Days.Day1
 ( part1
 , part2
 )
where

parseRotation :: String -> Integer
parseRotation ('L':n) = negate $ read n
parseRotation ('R':n) = read n
parseRotation _ = error "Invalid rotation string"

countZeroes :: (Integer, Integer) -> [Integer] -> Integer
countZeroes (_, count) [] = count
countZeroes (dial, count) (x:xs) = countZeroes (nextDial, nextCount) xs
  where
    nextDial = (dial + x) `mod` 100
    nextCount = count + (if nextDial == 0 then 1 else 0)

countWraps :: (Integer, Integer) -> [Integer] -> Integer
countWraps (_, count) [] = count
countWraps (dial, count) (x:xs) = countWraps (nextDial, nextCount) xs
  where
    nextDial = (dial + r) `mod` 100
    nextCount = count + abs q + (if wraps then 1 else 0)
    wraps = dial /= 0 && (dial + r < 1 || dial + r > 99)
    (q, r) = quotRem x 100

part1 :: [String] -> Integer
part1 d = countZeroes (50, 0) [parseRotation line | line <- d]

part2 :: [String] -> Integer
part2 d = countWraps (50, 0) [parseRotation line | line <- d]
