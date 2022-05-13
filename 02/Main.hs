import Data.Bool (bool)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Text.Parsec.String
import Text.Parsec

data Password = Password {
  minChar  :: Int,
  maxChar :: Int,
  letter :: Char,
  word :: String}
  deriving Show

testPassword :: Password -> Bool
testPassword (Password minCha maxChar char word) = a /= b
  where a = word !! (minChar - 1) == char
        b = word !! (maxChar - 1) == char

solve :: [Password] -> Int
solve = sum . map (bool 0 1) . map testPassword 

--fromFile :: FilePath -> -> IO Int
fromFile path f = f <$> parseFromFile (many1) path

line :: Parser Password
line = do
   minChar <- read @Int <$> many1 digit
   _ <- char
   maxChar <- read @Int <$> many1 digit
   _ <- char
   letter <- char
   word <- many1 char
   return $ Password minChar maxChar letter word