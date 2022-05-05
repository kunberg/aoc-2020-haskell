{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE LambdaCase #-}
import           Data.List (delete, nub)
import           Data.Map (Map)
import qualified Data.Map as M
import           Text.ParserCombinators.ReadP

data Direction = E | SE | SW | W | NW | NE deriving (Show, Eq)

data Tile = White | Black deriving (Show, Eq)

type Position = (Int, Int)

dirToPos :: Direction -> Position
dirToPos = \case
              E  -> ( 1, 0)
              SE -> ( 1,-1)
              SW -> ( 0,-1)
              W  -> (-1, 0)
              NW -> (-1, 1)
              NE -> ( 0, 1)

add (a1,b1) (a2,b2) = (a1+a2,b1+b2)

direction :: Parser Direction
direction = do
            c1 <- get
            case c1 of
              'e' -> return E
              'w' -> return W
              _   -> do
                     c2 <- get
                     case [c1,c2] of
                       "se" -> return SE
                       "sw" -> return SW
                       "nw" -> return NW
                       "ne" -> return NE

fromFile path f = f . map (parse $ greedy direction). lines <$> readFile path

startMap = foldl f [] . map (foldl1 add . map dirToPos)
  where f l a = if a `elem` l then delete a l else a:l

first =  length . startMap 

neighbours p = map (add p) [ ( 1, 0)
                           , ( 1,-1)
                           , ( 0,-1)
                           , (-1, 0)
                           , (-1, 1)
                           , ( 0, 1)
                           ]

turn :: Map Position Tile -> Map Position Tile
turn m = M.filter (== Black) $ M.mapWithKey calculate withWhites
  where blacks = M.keys m 
        whites = nub $ concatMap neighbours blacks
        withWhites = m <> M.fromList (zip whites (repeat White))
        calculate p t | t == White = if ns == 2           then Black else White
                      | t == Black = if ns == 0 || ns > 2 then White else Black
          where nps = neighbours p
                ns  = length . filter (== Just Black) $ map (withWhites M.!?) nps

second input = length $ iterate turn (M.fromList $ flip zip (repeat Black) $ startMap input) !! 100 


--- ReadP Helpers ---

type Parser = ReadP

parse p = fst . head . readP_to_S p

greedy :: Parser a -> Parser [a]
greedy p = do
  s <- look
  scan s
  where scan s = case readP_to_S p s of
                    []      -> return []
                    [(a,r)] -> do p; b <- scan r; return (a:b)
