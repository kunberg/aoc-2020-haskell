{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

import           Data.Char (isDigit)
import           Data.Containers.ListUtils (nubOrd)
import           Data.List (unfoldr, transpose)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, listToMaybe, mapMaybe )
import           Maybes (fromJust)
import           Text.ParserCombinators.ReadP




--- Types ---

data Pixel     = W
               | B
               deriving (Eq, Ord, Show)

data LabeledPixel = N
                  | Y Bool
                  deriving (Eq, Ord, Show)

toLabeled :: Pixel -> LabeledPixel
toLabeled = \case
               W -> N
               B -> Y False

data Borders t = Bs { a :: t,
                      r :: t,
                      b :: t,
                      l :: t}
               deriving (Eq, Ord, Functor, Foldable, Show)

type Tile a = (Int, a)

data Symmetry = Id
              | LeftRotation
              | DoubleRotation
              | RightRotation
              | VerticalFlip
              | HorizontalFlip
              | DiagonalFlip1
              | DiagonalFlip2
              deriving (Enum, Bounded)

apply :: Symmetry -> [[a]] -> [[a]]
apply = \case
           Id -> id
           LeftRotation   -> apply HorizontalFlip . apply DiagonalFlip1
           DoubleRotation -> apply LeftRotation   . apply LeftRotation
           RightRotation  -> apply LeftRotation   . apply DoubleRotation
           VerticalFlip   -> map reverse
           HorizontalFlip -> reverse
           DiagonalFlip1  -> transpose
           DiagonalFlip2  -> apply VerticalFlip . apply LeftRotation

--- Parsing ---

type Parser = ReadP

parse p = fst . last . readP_to_S p

pixel :: Char -> Pixel
pixel = \case
           '.' -> W
           '#' -> B

line :: Parser [Pixel]
line = map pixel <$> munch (`elem` ".#")

tile :: Parser (Tile [[Pixel]])
tile = do
       string "Tile "
       i <- read <$> munch isDigit
       string ":\n"
       ls <- endBy line (char '\n')
       return (i, ls)

file :: Parser [Tile [[Pixel]]]
file = sepBy tile (char '\n')



yoyo :: Parser [[Pixel]]
yoyo = endBy line (char '\n')

bildFile    = parse yoyo <$> readFile "bild"
monsterFile = parse yoyo <$> readFile "monster"
fromFile path f = f . parse file <$> readFile path

--- Helpers ---

labeledSymmetries :: [[a]] -> [(Symmetry, [[a]])]
labeledSymmetries b = map (\s -> (s, apply s b)) [minBound ..]


--- First Part ---

first :: [Tile [[Pixel]]] -> Int
first tilesPixels = product corners
  where sideLength     = floor . sqrt . fromIntegral $ length tilesPixels
        withSymmetries = concatMap (traverse labeledSymmetries) tilesPixels
        borderTiles    = (fmap.fmap.fmap) (\ ls -> Bs (head ls) (map last ls) (last ls) (map head ls)) withSymmetries
        solution       = solveFirst sideLength borderTiles
        corners        = map (fst . (solution !!))
                         [ 0                             -- top left
                         , sideLength -1                 -- top right
                         , sideLength * (sideLength - 1) -- bottom left
                         , sideLength * sideLength - 1   -- bottom right
                         ]

solveFirst len ts  = fromJust $ solveFirst' len ts []
  where solveFirst' _          []    result = Just result
        solveFirst' sideLength tiles result = (listToMaybe . mapMaybe  fit) tiles
          where fit t@(i,(_,border)) = if   right && below
                                       then solveFirst' sideLength (filter ((i /=) . fst) tiles) (t:result)
                                       else Nothing
                                       where right = length result `mod` sideLength == 0 ||
                                                     r border == l (snd . snd . head $ result)
                                             below = length result < sideLength ||
                                                     b border == a (snd $ snd (result !! (sideLength-1)))


--- Second Part ---

second :: [Tile [[Pixel]]] -> Int
second tilesPixels = minimum $ map (solveSecond . (map.map) toLabeled . (`apply` picture)) [minBound ..]
  where sideLength     = floor . sqrt . fromIntegral $ length tilesPixels
        withSymmetries = concatMap (traverse labeledSymmetries) tilesPixels
        borderTiles    = (fmap.fmap.fmap) (\ ls -> Bs (head ls) (map last ls) (last ls) (map head ls)) withSymmetries
        getPixels      = (M.fromList tilesPixels M.!)
        removeBorders  = init . tail . map (init . tail)
        pictureAnl     = (\(i,(s,_)) -> removeBorders $ apply s $ getPixels i)
                         <$> solveFirst sideLength borderTiles
        chunks         = takeWhile (not . null) . unfoldr (Just . splitAt sideLength)
        torben xs@(s:_) | null s    = []
                | otherwise = concatMap head xs : torben (map tail xs)
        picture        = concatMap torben (chunks pictureAnl)


solveSecond :: [[LabeledPixel]] -> Int
solveSecond picture = let markedPicture = foldl testung picture [(x,y) | x <- [0..length picture],
                                                                         y <- [0..length picture]]
                      in  length $ filter (== Y False) $ concat markedPicture


testung :: [[LabeledPixel]] -> (Int, Int)  -> [[LabeledPixel]]
testung picture (x,y) = let (pl, nl) = splitAt x picture
                            rest     = case nl of
                                         (a:b:c:r) -> monsterRows y [a,b,c] ++ r
                                         r         -> r

                        in  pl ++ rest

monsterRows y  rs = let nrs = map (komische y) rs
                        monster = markMonster $ map (\(_,m,_) -> m) nrs
                 in  zipWith (\(s,_,r) m -> s++m++r) nrs monster
komische y r = let (st, wo) = splitAt y  r
                   (mo, re) = splitAt 20 wo
               in  (st,mo,re)




markMonster [[a01,a02,a03,a04,a05,a06,a07,a08,a09,a10,a11,a12,a13,a14,a15,a16,a17,a18,Y _,a20],
             [Y _,b02,b03,b04,b05,Y _,Y _,b08,b09,b10,b11,Y _,Y _,b14,b15,b16,b17,Y _,Y _,Y _],
             [c01,Y _,c03,c04,Y _,c06,c07,Y _,c09,c10,Y _,c12,c13,Y _,c15,c16,Y _,c18,c19,c20]] =

            [[a01,a02,a03,a04,a05,a06,a07,a08,a09,a10,a11,a12,a13,a14,a15,a16,a17,a18,Y o,a20],
             [Y o,b02,b03,b04,b05,Y o,Y o,b08,b09,b10,b11,Y o,Y o,b14,b15,b16,b17,Y o,Y o,Y o],
             [c01,Y o,c03,c04,Y o,c06,c07,Y o,c09,c10,Y o,c12,c13,Y o,c15,c16,Y o,c18,c19,c20]]
            where o = True
markMonster m = m




--- Main ---

main :: IO ()
main = do
   i <- fromFile "input.txt"  second
   print i
