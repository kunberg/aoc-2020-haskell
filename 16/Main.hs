--{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}

import Text.Parsec.String
import Text.Parsec
import Control.Applicative (liftA2)
import Control.Monad (void)
import Data.Either (fromRight, isLeft, fromLeft)
import Data.Foldable (fold)
import Data.List (isPrefixOf, (\\), transpose, minimumBy, nub, sortOn, maximumBy, find)
import Data.Containers.ListUtils (nubOrd)
import Data.Ord (comparing)
import Data.Maybe (fromJust)




type Field = (String, [Int])

type Ticket = [Int]

data Input = Input { fields :: [Field]
                   , myTicket :: Ticket
                   , nearbyTickets :: [Ticket]
                   }

input :: Parser Input
input = do
  fs  <- manyTill field $ char '\n'
  void $ string "your ticket:\n"
  mt  <- ticket
  void $ string "\nnearby tickets:\n"
  nts <- manyTill ticket eof
  return $ Input fs mt nts

field :: Parser Field
field = do
  s <- manyTill anyChar $ char ':'
  void $ char ' '
  a <- read <$> many1 digit
  void $ char '-'
  b <- read <$> many1 digit
  void $ string " or "
  c <- read <$> many1 digit
  void $ char '-'
  d <- read <$> many1 digit
  void $ char '\n'
  return (s, [a..b] ++ [c..d])

ticket :: Parser Ticket
ticket = do
  ts <- (read <$> many1 digit) `sepBy1` char ','
  void $ char '\n'
  return ts

fromFile path f = fmap f <$> parseFromFile input path


first (Input fs _ ts) = sum . filter (`notElem` fns) $ concat ts
  where fns = snd $ foldl1 (liftA2 (++)) fs

second i@(Input _ mt _) = product .
                          fmap snd .
                          filter (\(x,_) -> elem x $ filter (\x -> "departure" `isPrefixOf` x) fs) .
                          (`zip` mt) $
                          fs
  where fs = solveSecond i


solveSecond i@(Input fs _ _) = map (fromRight $ error "ouch") . test . map Left $ fits fs ts
  where ts = transpose $ validTickets i

fits = fits' []
  where fits' as fs (is:iss) = fits' (as ++ [function is fs]) fs iss
          where function is = map fst . filter (\(_,fis) -> all (`elem` fis) is)
        fits' as _ [] = as

test :: [Either [String] String] -> [Either [String] String]
test es | any isLeft es = test $ function s es
        | otherwise     = es
  where s = head . fromLeft (error "Left") . fromJust $ find (\case Left [l] -> True; _ -> False) es

function :: String -> [Either [String] String] -> [Either [String] String]
function = function' []
  where function' r s (Left e:es) | length e == 1 && s `elem` e = function' (r ++ [Right s]) s es
                                  |                  s `elem` e = function' (r ++ [Left (e \\ [s])]) s es
        function' r s (right:es) = function' (r ++ [right]) s es
        function' r _ []         = r

validTickets (Input fs _ ts) = filter (all (`elem` fns)) ts
  where fns = snd $ foldl1 (liftA2 (++)) fs
