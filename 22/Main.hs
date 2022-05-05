import Data.List ((\\))
fromFile path f = uncurry f .
                  (\(a,b) -> (map read a, map read $ tail b)) .
                  break (== "") . (\\ ["Player 1:", "Player 2:"]) .
                  lines <$> readFile path

first a       []    = score a
first []      b     = score b
first (a:as) (b:bs) = if a > b then first (as ++ [a,b]) bs else first as (bs ++ [b,a])

score l = sum $ zipWith (*) [1..] (reverse l)

second = second' []

second' acc a b | (a,b) `elem` acc = score a
second' _ a       []    = score a
second' _ []      b     = score b
second' acc (a:as) (b:bs) | a <= length as && b <= length bs = if secondr [] (take a as) (take b bs)
                                                               then second' ((a:as,b:bs):acc) (as ++ [a,b]) bs
                                                               else second' ((a:as,b:bs):acc) as (bs ++ [b,a])

                          | otherwise                            = if a > b
                                                                   then second' ((a:as,b:bs):acc) (as ++ [a,b]) bs
                                                                   else second' ((a:as,b:bs):acc) as (bs ++ [b,a])

secondr :: [([Int], [Int])] -> [Int] -> [Int] -> Bool
secondr acc a b | (a,b) `elem` acc = True
secondr _  a       []    = True
secondr _ []      b     = False
secondr acc (a:as) (b:bs) | a <= length as && b <= length bs = if secondr [] (take a as) (take b bs)
                                                               then secondr ((a:as,b:bs):acc) (as ++ [a,b]) bs
                                                               else secondr ((a:as,b:bs):acc) as (bs ++ [b,a])

                          | otherwise                            = if a > b
                                                                   then secondr ((a:as,b:bs):acc) (as ++ [a,b]) bs
                                                                   else secondr ((a:as,b:bs):acc) as (bs ++ [b,a])
