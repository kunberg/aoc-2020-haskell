import Data.List (elemIndex)
import Data.Maybe (fromJust)

tcard = 5764801  :: Int
tdoor = 17807724 :: Int

card = 10441485 :: Int
door = 1004920  :: Int

initial = 7        :: Int
divisor = 20201227 :: Int

transform :: Int -> Int -> Int
transform s v = (s * v) `mod` divisor

decrypt :: Int -> Int
decrypt pk = fromJust $ elemIndex pk $ iterate' (transform initial) 1

first card door = iterate' (transform card) 1 !! decrypt door

iterate' f x =  x : (iterate' f $! f x)
