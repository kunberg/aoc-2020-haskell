{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import Data.List ((\\))
import Data.Map.Internal (notMember)

data Status   = Active | Inactive deriving (Show,Eq, Ord)

type Position = (Int,Int,Int,Int)

type Plan = Map Position Status


second plan = length . M.filter (== Active) $ iterate turn plan !! 6

fromFile path f = f . toPlan .  (map.map) parse . lines <$> readFile path

parse = \case
           '#' -> Active
           '.' -> Inactive

toPlan :: [[Status]] -> Plan
toPlan rows = M.fromList $ do
                           (x, row)    <- zip [0..] rows
                           (y, status) <- zip [0..] row
                           return ((x,y,0,0), status)

status plan pos = fromMaybe Inactive $ M.lookup pos plan

turn :: Plan -> Plan
turn plan = M.mapWithKey (\p s-> if | activeNeighbors p plan == 3                -> Active
                                    | activeNeighbors p plan == 2 && s == Active -> Active
                                    | otherwise                                  -> Inactive
                         ) $ expand $ M.filter (== Active) plan

expand :: Plan -> Plan
expand plan = plan <> M.fromList (concatMap expand1 $ M.keys plan)
  where expand1 pos = [(npos, Inactive) | npos <- allNeighbors pos, notMember npos plan]


allNeighbors :: Position -> [Position]
allNeighbors p@(x,y,z,w) = [(a,b,c,d) |a <- [x-1..x+1],
                                       b <- [y-1..y+1],
                                       c <- [z-1..z+1],
                                       d <- [w-1..w+1],
                                       not (a == x && b == y && c == z && d == w)]

activeNeighbors :: Position -> Plan -> Int
activeNeighbors pos plan = length . filter (== Active) . map (status plan) $ allNeighbors pos
