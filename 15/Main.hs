import Data.List (find, elemIndex)
import Data.Maybe (fromJust)

import Data.Map (Map)
import qualified Data.Map as M

input :: [Int]
input = [2, 16, 0, 1, 12, 7]

test :: [Int]
test = [6, 3, 0]
  
first i = case length i of
            2020 -> head $ i
            _    -> first $ (solveFirst i) : i

solveFirst (i:is) = case find (== i) is of
                      Nothing -> 0
                      Just x  -> 1 + (fromJust $ elemIndex x is)

second (i:is) = solveSecond (length $ i:is) i (M.fromList $ flip zip [1..] $ reverse is)

solveSecond :: Int -> Int -> Map Int Int -> Int
solveSecond t n m = case t of
                      29999999 -> x
                      _    -> solveSecond (t+1) x $! M.insert n t m
                      where x = case m M.!? n of
                                  Nothing -> 0
                                  Just o  -> t - o
