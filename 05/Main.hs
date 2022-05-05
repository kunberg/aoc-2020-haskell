import Numeric
import Data.List

fromFile path = sort <$> map solve <$> map (splitAt 7) <$> lines <$> readFile path

solve :: (String, String) -> Int
solve (bf, rl) = (fst $ head $ parse ('B', 'F') bf) * 8 + (fst $ head $ parse ('R', 'L') rl)

parse (i, o) = readInt 2 (\x -> x == i || x == o) (\x -> if x == i then 1 else 0)

missing :: [Int] -> Int
missing (x:y:zs) | x + 1 == y = missing (y:zs)
                 | otherwise  = x + 1
