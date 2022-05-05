{-# LANGUAGE OverloadedLists#-}

import           Data.Foldable ( toList)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Sequence (Seq((:<|), (:|>)))
import qualified Data.Sequence as S


type Turn = (Int, Map Int (Seq Int), Seq Int, Seq Int)


firstTestTurn :: Turn
firstTestTurn = (9, M.empty, [], [3,8,9,1,2,5,4,6,7])

firstTurn :: Turn
firstTurn = (9, M.empty, [], [4,6,9,2,1,7,5,3,8])

secondTestTurn :: Turn
secondTestTurn = (1000000, M.empty, [], [3,8,9,1,2,5,4,6,7] <> [10..1000000])

secondTurn :: Turn
secondTurn = (1000000, M.empty, [], [4,6,9,2,1,7,5,3,8] <> [10..1000000])


first t = (\(_,m,a,c) -> insertAll m (c <> a)) $ iterate' turn t !! 100

insertAll m seq | m == M.empty = seq
                | otherwise    = uncurry insertAll $ insert m seq

second t = solveSecond $ iterate' turn t !! 10000000

main = print $ second secondTurn

solveSecond :: Turn -> Int
solveSecond (len, m, a, c) = solveSecond' (m, c<>a)
  where solveSecond' (m, s:<|c ) | s == 1    = product $ S.take 2 result
                                               where result = case m M.!? s of
                                                                Nothing -> secondLevel c
                                                                Just x  -> secondLevel x
                                                              where secondLevel (n1:<|re) = case m M.!? n1 of
                                                                                              Nothing -> n1 :<| re
                                                                                              Just x  -> n1 :<| x
                                                                    secondLevel _ = error "unreachable"
        solveSecond' (m, s:<|c ) = solveSecond' (dm, nc)
                                   where nc = S.fromList (concatMap toList $ m M.!? s) <> c
                                         dm = M.delete s m
        solveSecond' _ = error "unreachable"
                                         
turn :: Turn -> Turn
turn (len, m, a, []) = turn (len, m, [], a)
turn (len, m, a, c ) | length c < 4 = turn (len, nm, [], nc)
                                      where (nm, nc) = insert m (c <> a)
turn (len, m, a, c ) = (len, am, a:|>cur, ncr)
                       where (cc, cr)        = S.splitAt 4 c
                             (dm, nc)        =  (<> cr) <$> insert m cc
                             (cur:<|ps, ncr) =  S.splitAt 4 nc
                             add             = add' len
                             d               = d' cur
                             d' x            = if   add (-1) x `elem` ps
                                               then d'  $ add (-1) x
                                               else add (-1) x
                             am              = M.insert d ps dm

insert n []        = (n,[])
insert n (s:<|seq) = case n M.!? s of
                       Nothing -> (nn, s :<| ns)
                                   where (nn, ns) = insert n seq
                       Just is -> (<> seq) <$> insert (M.delete s n) (s :<| is)
insert _ _ = error "unreachable"

iterate' :: (a -> a) -> a -> [a]
iterate' f a =  a : (iterate' f $! f a)

add' :: Int -> Int -> Int -> Int
add' len a i | a > 0 = if   i + a > len
                       then i + a - len
                       else i + a
             | a < 0 = if   i + a < 1
                       then i + a + len
                       else i + a
             | otherwise = error "unreachable"
