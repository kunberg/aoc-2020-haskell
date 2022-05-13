import Control.Monad (liftM)

fromFile f path = f <$> parse <$> readFile path

parse = map (read @Int) . lines

two x (n:ns) = if any (== (x - n)) ns
               then Just (n, (x - n))
               else two x ns
two _ _ = Nothing

first = liftM (uncurry (*)) . two 2020

second (n:ns) = case two (2020 - n) ns of
                    Nothing -> second ns
                    Just (a, b) -> Just (a * b * n)
second _ = Nothing