import           Data.List (sort)
import qualified Data.Map as M

fromFile path f = (f . sort . map read . lines) <$> readFile path


first l = (diffs M.! 1) *  (diffs M.! 3)
  where diffs = solveFirst (M.fromList $ map (\x -> (x,0)) [1..3]) (0:l)

solveFirst diffs (x:y:zs) = solveFirst newDiffs (y:zs)
  where newDiffs = M.adjust (+ 1) (y - x) diffs                              
solveFirst diffs _ = M.adjust (+ 1) 3 diffs
  
second = solveSecond 3 . ((0,1) :) . map (\x -> (x, 0)) 

solveSecond i (x:xs) | xs == []  = snd x
                     | otherwise = solveSecond i ( map fit (take i xs) ++ drop i xs)
                        where fit y = if (fst y) - (fst x) <= 3 then (fst y, snd y + snd x) else y
