import           Control.Monad (void, liftM)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (isJust)
import           Text.Parsec
import           Text.Parsec.String
import Maybes (isNothing, rightToMaybe)


data Rule = Char Char
          | One  [Int]
          | Two  [Int] [Int]
          | R8
          | R11
          deriving Show

type Rules = Map Int Rule

type Message = String

fromFile :: FilePath -> (Rules -> [Message] -> a) -> IO (Either ParseError a)
fromFile path f = fmap (uncurry f) <$> parseFromFile input path

input :: Parser (Rules, [Message])
input = do
  rules <- manyTill rule newline
  messages <- endBy (many $ noneOf "\n") newline
  return (M.fromList rules , messages)

rule :: Parser (Int, Rule)
rule = do
  n <- read <$> many1 digit
  string ": "
  r <- character <|> try two <|> try one
  newline
  return (n, r)

one :: Parser Rule
one = do
  ns <- sepBy (read <$> many digit) (char ' ')
  return $ One ns

two :: Parser Rule
two = do
  ns <- endBy (read <$> many digit) (char ' ')
  string "| "
  ms <- sepBy (read <$> many digit) (char ' ')
  return $ Two ns ms

character :: Parser Rule
character = do
  char '\"'
  c <- anyChar
  char '\"'
  return $ Char c

first :: Rules -> [Message] -> Int
first r ms = length . filter id $ map (check r) ms

second :: Rules -> [Message] -> Int
second r ms = length . filter id $ map (check nr) ms
  where nr = M.insert 8 R8 $ M.insert 11 R11 r 

check :: Rules -> Message -> Bool
check r m = let One (a:as) = (r M.! 0)
            in case check' r (Just m) a of
              Nothing -> False
              Just nm -> case as  of
                           [] -> null nm
                           x  -> check (M.insert 0 (One x) r) nm
  where check' :: Rules -> Maybe Message -> Int -> Maybe Message
        check' r mes@(Just (m:ms)) i = case r M.! i of
                              Char c     -> if c == m then Just ms else Nothing                              
                              Two is js  -> if isJust mis then mis else mjs
                                where mis = foldl (check' r) mes is
                                      mjs = foldl (check' r) mes js
        check' _ Nothing _  = Nothing

beforeFirstNothing :: Maybe a -> [Maybe a] -> Maybe a
beforeFirstNothing ma (a:as) = if isNothing a then ma else beforeFirstNothing a as  
                              
