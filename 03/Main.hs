import Data.Monoid

type Map = [(Int, [Bool])]

angles :: [(Int, Int)]
angles = [ (1, 1)
         , (3, 1)
         , (5, 1)
         , (7, 1)
         , (1, 2)
         ]

fromFile :: FilePath -> IO Int
fromFile path = solve angles <$> zipWith (,) [0..] <$> map (cycle . map (== '#')) <$> lines <$> readFile path

solve :: [(Int, Int)] -> Map -> Int
solve a m = product $ map getSum $ map (solveMap m) a

solveMap :: Map -> (Int, Int) -> Sum Int
solveMap m a = foldMap (solveLine a) m

solveLine :: (Int, Int) -> (Int, [Bool]) -> Sum Int
solveLine (column, row) (i, l) = if (mod i row == 0) && (l !! div (i * column) row) then 1 else 0
