fromFile path f = (f . map read . lines) <$> readFile path

first n = solveFirst . splitAt n

solveFirst (p:ps, n:ns) = case elem n (possible $ p:ps ) of
                       True  -> solveFirst (ps ++ [n], ns)
                       False -> n
                     where possible [] = []
                           possible (x:xs) = map (+ x) xs ++ possible xs

second n l = solveSecond (first n l) l

solveSecond n l@(x:xs) = case trySecond 1 0 n l of
                           Just i  -> minimum (take i l) + maximum (take i l)
                           Nothing -> solveSecond n xs

trySecond i count goal (x:xs) | count + x >  goal = Nothing
                              | count + x == goal = Just i
                              | otherwise         = trySecond (i + 1) (count + x) goal xs
trySecond _ _ _ _ = Nothing
