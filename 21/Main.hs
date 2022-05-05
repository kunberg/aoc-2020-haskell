import           Data.List (nub, intersect, (\\), sort, intercalate, find)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Tuple (swap)

fromFile path f = f . map (mf . break (== '(')) . lines <$> readFile path
  where mf (is,as) = (words is, words $ drop 9 $ filter (not . (`elem` "(),")) as)

first l = length $ filter (`elem` nas) is
  where m   = M.fromListWith intersect . map swap $ concatMap sequence l
        is  = concatMap fst l
        nas = nub is \\ concat (M.elems m)

second l = intercalate "," $ concatMap (resultm M.!) (sort $ M.keys resultm)
  where m       = M.fromListWith intersect . map swap $ concatMap sequence l
        resultm = resultm' M.empty m :: Map String [String]
        resultm' result input = case find ((1 ==) . length) input of
                                  Nothing -> result
                                  Just f  -> uncurry resultm' $ M.foldlWithKey fun (result, M.empty) input
                                    where fun (r,acc) k is = if f == is
                                                             then (M.insert k is r, acc)
                                                             else (r, M.insert k (is \\ f) acc)
