import qualified Data.Set as Set

fromFile k x path = solve k x <$> map read <$> lines <$> readFile path

solve k x = map product . filter ((x ==) . sum) . choose k

choose 1    es  = map Set.singleton es 
choose k (e:es) = map (Set.insert e) (choose (k-1) es) ++ choose k es
choose _ _      = []
