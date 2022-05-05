import           Data.Bits ((.|.), (.&.), xor)
import           Data.Char (isDigit, digitToInt, intToDigit)
import           Data.List (delete, nubBy, nub)
import           Data.Map (Map)
import qualified Data.Map as M
import           Numeric (showIntAtBase, readInt)


data Line = Mask Mask
          | Assign Assign
          deriving Show

type Mask = (Word, Word, [Word])
type Assign = (Word, Word)

fromFile path f = f . map (parseLine . delete "=" . words) . lines <$> readFile path

parseLine :: [String] -> Line 
parseLine [a,b] = case a of
                    "mask" -> Mask ((parseS (parseB 0) b), (parseS (parseB 1) b), nub $ digits $ parseS (\x -> if x == 'X' then 1 else 0) b)
                      where parseS f   = foldl (\acc x -> acc * 2 + f x) 0
                            parseB n x = case x of
                                          'X' -> n
                                          '1' -> 1
                                          '0' -> 0
                            digits = zipWith (*) [(2^i) | i <- [0..]] . reverse . map (fromIntegral . digitToInt) . ($ "") . showIntAtBase 2 intToDigit 
                    _      -> curry Assign (read $ filter isDigit a) (fromIntegral $ read b)

first = snd . foldl solveFirst (Mask (0, 0, []), 0) . reverse . nubBy eq . reverse
  where eq (Mask _) _ = False
        eq _ (Mask _) = False
        eq (Assign (a,_)) (Assign (b,_)) = a == b

solveFirst :: (Line, Word) -> Line -> (Line, Word)
solveFirst (_, acc) m@(Mask _) = (m, acc)
solveFirst (m, acc) (Assign (_,w)) = (m, acc + apply m w)   
  where apply (Mask (i, o, _)) w = (w .|. i) .&. o



second :: [Line] -> Word
second = foldl1 (+) . snd . foldl solveSecond (Mask (0, 0, []), M.empty)

solveSecond :: (Line, Map Word Word) -> Line -> (Line, Map Word Word)
solveSecond (_, acc) m@(Mask _) = (m, acc)
solveSecond (m@(Mask (_, _, x)), acc) (Assign (a, w)) = (m, foldl (\ma x -> M.insert x w ma) acc (nub $ k x [apply m a]))
  where apply (Mask (i, _, _)) w = (w .|. i)

k :: [Word] -> [Word] -> [Word]
k (x:xs) ys = k (xs) $ map (xor x) ys ++ ys
k [] ys    = ys

binary :: Word -> String
binary = ($ "") . showIntAtBase 2 intToDigit
