{-# LANGUAGE OverloadedStrings #-}

import  Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.Read as T
import Data.Bool (bool)
import Data.Monoid (getSum)
import Data.Map (Map)
import qualified Data.Map as M
import Text.Read
import Data.Char

fields :: [Text]
fields = [ "byr"
         , "iyr"
         , "eyr"
         , "hgt"
         , "hcl"
         , "ecl"
         , "pid"
         ]

checkFields :: Map Text String -> Bool
checkFields  = flip (all . flip elem) fields . M.keys  

checkByr m = case byr of
               Just i ->  i >= 1920 && i <= 2002
               Nothing -> False
  where byr = (readMaybe $ m M.! "byr") :: Maybe Int  
  

checkIyr m = case iyr of
               Just i ->  i >= 2010 && i <= 2020
               Nothing -> False
  where iyr = (readMaybe $ m M.! "iyr") :: Maybe Int


checkEyr m = case eyr of
               Just i ->  i >= 2020 && i <= 2030
               Nothing -> False
  where eyr = (readMaybe $ m M.! "eyr")


checkHgt m = case hgt of
             a : b : c : "cm" -> case y of
                                   Just i -> i >= 150 && i <= 193
                                   Nothing -> False
               where y = readMaybe (a:b:c:[])
             a : b : "in" -> case y of
                                   Just i -> i >= 59 && i <= 76
                                   Nothing -> False
               where y = readMaybe (a:b:[])
             _        -> False
  where hgt = m M.! "hgt"

checkEcl m =  ecl == "amb"
           || ecl == "blu"
           || ecl == "brn"
           || ecl == "gry"
           || ecl == "grn"
           || ecl == "hzl"
           || ecl == "oth"
  where ecl = m M.! "ecl"
  
checkPid m = length pid == 9 && all isDigit pid
  where pid = m M.! "pid" 

checkHcl m = case hcl of
             '#' : rs -> length rs == 6 && all isAlphaNum rs
             _        -> False
  where hcl = m M.! "hcl"

solve m =  checkFields m
           && checkByr m           
           && checkIyr m
           && checkEyr m
           && checkHgt m
           && checkHcl m
           && checkEcl m
           && checkPid m

fromFile path =     getSum
                <$> foldMap (bool 0 1)
                <$> map solve
                <$> (map . M.map) (tail . T.unpack)
                <$> map M.fromList 
                <$> map ((map $ T.breakOn ":") . T.words)
                <$> T.splitOn "\n\n"
                <$> T.readFile path 



