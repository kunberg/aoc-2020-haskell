import           Data.Foldable (fold)
import           Data.Sequence (Seq)
import qualified Data.Sequence as S

data Space = Floor
           | Empty
           | Taken
           deriving (Eq, Show)

parse c = case c of
            '.' -> Floor
            'L' -> Empty
            '#' -> Taken

fromFile path f = (f . (fmap . fmap) parse . S.fromList . map S.fromList . lines) <$> readFile path 


first = length . S.filter (== Taken) . fold . solveFirst

solveFirst :: Seq (Seq Space) -> Seq (Seq Space)
solveFirst layout = case layout == new layout of
                      True  -> layout
                      False -> solveFirst $ new layout
                    where new l = S.mapWithIndex (S.mapWithIndex . change l) l
                          find      l (a,b) = case (S.lookup b) =<< (S.lookup a l) of
                                                Just Taken -> True
                                                otherwise  -> False
                          neighbors l a b   = length $ filter (find l) [(x,y) | x <- [a - 1 .. a + 1], y <- [b - 1 .. b + 1], (x,y) /= (a,b)]
                          change    l a b s = case s of
                                                Floor -> Floor
                                                Empty -> if (neighbors l a b) == 0 then Taken else Empty
                                                Taken -> if (neighbors l a b) >= 4 then Empty else Taken


second = length . S.filter (== Taken) . fold . solveSecond

solveSecond :: Seq (Seq Space) -> Seq (Seq Space)
solveSecond layout = case layout == new layout of
                      True  -> layout
                      False -> solveSecond $ new layout
                    where new l = S.mapWithIndex (S.mapWithIndex . change l) l
                          find      l (a,b) = (S.lookup b) =<< (S.lookup a l)
                          change    l a b s = case s of
                                                Floor -> Floor
                                                Empty -> if (neighbors l a b) == 0 then Taken else Empty
                                                Taken -> if (neighbors l a b) >= 5 then Empty else Taken
                          neighbors l a b = length $ filter (== True) $ fmap yolo [(x,y)| x <- [-1 .. 1], y <- [-1 .. 1], (x,y) /= (0,0)]
                            where yolo (x, y) = case find l (a+x, b+y) of
                                                  Nothing    -> False
                                                  Just Taken -> True
                                                  Just Empty -> False
                                                  Just Floor -> yolo (a+x, b+y)
