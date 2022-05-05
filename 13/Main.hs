{-# LANGUAGE OverloadedStrings #-}

import           Data.List (intersect)
import           Data.Maybe (fromJust)
import qualified Data.Text as T

fromFile path f = (f . (\[a,b] -> (read a, (T.splitOn "," . T.pack) b)) . lines) <$> readFile path


first = (\ (a, b) -> a * b) . solveFirst . fmap (map read . filter (/= "x") . map T.unpack)

solveFirst :: (Int, [Int])-> (Int, Int)
solveFirst (t, ls) = (number - mod t number, number)
  where number = foldl (\ a b -> if rest a < rest b then a else b) (t-1) ls
        rest n = n - mod t n


second :: (Int, [T.Text]) -> Int
second =  solveSecond . (map . fmap) (read . T.unpack) . filter (\x -> snd x /= "x") . zip [0..] . snd

solveSecond :: [(Int, Int)] -> Int
solveSecond l = (\ (a,b) -> b - a) $ foldl1 fits l

fits :: (Int, Int) -> (Int, Int) -> (Int, Int)
fits (d1, t1) (d2, t2) = (t1 * t2 - n, t1 * t2) 
  where ((a,_),g) = euclid t1 t2
        c         = t2 - t1 + d1 - d2
        e = mod (a * (div c g)) t2
        n = [t1 - d1 , t1 + (t1 - d1) ..] !! e

euclid :: Int -> Int -> ((Int, Int), Int)
euclid a b = euclid' [] (max a' b') (min a' b')
   where (a', b') = (abs a, abs b)
         euclid' qs n r = case r of
                            0 -> (if a' > b' then (ea, eb) else (eb, ea) , n)
                              where (ea,  eb)  = (signum a * ea', signum b * eb')
                                    (ea', eb') = extend $ tail qs
                            _ -> euclid' (d:qs) r m
                              where (d, m) = divMod n r

         extend = (\(_, _, x, y) -> (x,y)) . foldl1 multiply . map (\q -> (0, 1, 1, -q))
         multiply (a, b, c, d) (k, l, m, n) = (a*k+b*m, a*l+b*n, c*k+d*m, c*l+d*n)



-- (x + 1) * 35 - 0 = (y + 1) * 61 -1
-- 35 * x + 35      = 61 * y + 60
-- 35 * x - 61 * y  = 25
-- 35 * (25 * 7) - 61 * (25 * (- 4))
