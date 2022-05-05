import Data.Bool (bool)
import qualified Data.Text as T
import qualified Data.Text.IO as T


data Password = Password {
  first  :: Int,
  second :: Int,
  char :: Char,
  word :: String}
  deriving Show

testPassword :: Password -> Bool
testPassword (Password first second char word) = a /= b
  where a = word !! (first - 1) == char
        b = word !! (second - 1) == char

solve :: [Password] -> Int
solve = sum . map (bool 0 1) . map testPassword 

fromFile :: FilePath -> IO Int
fromFile path = solve <$> map parseLine <$> T.lines <$> T.readFile path

parseLine :: T.Text -> Password
parseLine input = Password  first second (T.head char) (T.unpack string)
   where [numbers, char, string] = T.words input
         first  = read $ T.unpack $ (T.splitOn (T.pack "-") numbers) !! 0
         second = read $ T.unpack $ (T.splitOn (T.pack "-") numbers) !! 1
