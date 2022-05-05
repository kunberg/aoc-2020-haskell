{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.List


fromFile path = sum <$> map length <$> map (foldl intersect ['a'..'z']) <$> map lines <$> map T.unpack <$> T.splitOn "\n\n" <$> T.readFile path 
