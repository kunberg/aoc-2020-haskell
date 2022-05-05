{-# LANGUAGE LambdaCase, MultiWayIf #-}

import Data.Containers.ListUtils (nubOrd)
import Data.List ((\\), find)
import Data.Maybe (fromMaybe)

data Status   = Active | Inactive deriving (Show,Eq, Ord)
type Position = (Int, Int, Int)
type Cube     = (Status, Position)

type Position4 = (Int, Int, Int, Int)
type Cube4     = (Status, Position4)

fromFile path f = f . (map.map) parse . lines <$> readFile path


parse = \case
           '#' -> Active
           '.' -> Inactive

isActive (s,_) = s == Active

listed p = any (\(_,x) -> p == x)

getCube m p = fromMaybe (Inactive, p) (find (\(_,x) -> x == p) m)



first m = length . filter isActive $ iterate turn (toCubes m) !! 6

toCubes :: [[Status]] -> [Cube]
toCubes rows = do
  (x, row)    <- zip [0..] rows
  (y, status) <- zip [0..] row
  return (status, (x,y,0))

turn :: [Cube] -> [Cube]
turn m = map (\(s, p) -> if | activeNeighbors p m == 3                 -> (Active  , p)
                             | activeNeighbors p m == 2 && s == Active -> (Active  , p)
                             | otherwise                                -> (Inactive, p)
                      ) $ expand $ filter isActive m

expand :: [Cube] -> [Cube]
expand m = m ++ nubOrd (concatMap expand1 m)
  where expand1 :: Cube -> [Cube]
        expand1 (_, p) = [(Inactive, np) | np <- allNeighbors p, not $ listed np m]

allNeighbors :: Position -> [Position]
allNeighbors p@(x,y,z) = [(a,b,c) |a <- [x-1..x+1], b <- [y-1..y+1], c <- [z-1..z+1]] \\ [p]

activeNeighbors :: Position -> [Cube] -> Int
activeNeighbors p m = length . filter isActive . map (getCube m) $ allNeighbors p
