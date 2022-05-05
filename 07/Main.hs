{-# LANGUAGE OverloadedStrings #-}

import           Data.Char (isSpace)
import           Data.Map (Map, (!), fromList)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Tuple

data Edge n e  = Edge n e

type Graph n e = Map n [Edge n e]


fromFile path = (flip solve) "shinygold"
                         <$> fromList
                         <$> (map . fmap  . map) (uncurry Edge)
                         <$> (map . fmap  . map  . fmap) (read . T.unpack)
                         <$> (map . fmap . map) swap 
                         <$> (map . fmap . map) (T.splitAt 1)
                         <$> (map . fmap) (\x -> if x == ["noother"] then [] else x)
                         <$> (map . fmap) (T.splitOn ",")
                         <$> (map . fmap) (T.drop 7)
                         <$> map (T.breakOn "contain")
                         <$> map (T.filter $ not . isSpace)
                         <$> map T.concat
                         <$> map (T.splitOn " bag")
                         <$> map T.concat
                         <$> map (T.splitOn " bags")
                         <$> map (T.dropEnd 1)
                         <$> T.lines
                         <$> T.readFile path

solve :: Graph Text Int -> Text -> Int
solve graph start = case edges of
                      [] -> 1
                      _  -> 1 + (sum $ map next edges)
   where edges = graph ! start
         next (Edge l i) = i * (solve graph l)
         
  





