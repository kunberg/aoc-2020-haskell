import           Data.Char (isDigit)
import           Data.Map (Map)
import qualified Data.Map as M
import           Text.ParserCombinators.ReadP
import Data.List (nub)


newtype Rules = Rules {getRules :: Map Int (Rules -> Parser ())}

type Message = String

fromFile :: FilePath -> (Rules -> [Message] -> a) -> IO a
fromFile path f = uncurry f . fst . head . parse input <$> readFile path

input :: Parser (Rules, [Message])
input = do
  rules <- greedy rule
  char '\n'
  messages <- greedy $ do
                       m <- munch (/= '\n')
                       char '\n'
                       return m
  return (Rules $ M.fromList rules, messages)

rule :: Parser (Int, Rules -> Parser ())
rule = do
  n <- many1 $ satisfy isDigit
  string ": "
  r <- character +++ two +++ one
  char '\n'
  return (read n,r)

one :: Parser (Rules -> Parser ())
one = do
  ns <- sepBy (many $ satisfy isDigit)  (char ' ')
  return (\(Rules r) -> do
    foldl1 (>>) $ map ((($ Rules r) . (r M.!)) . read) ns
    return ())

two :: Parser (Rules -> Parser ())
two = do
  ns <- endBy (many $ satisfy isDigit) (char ' ')
  string "| "
  ms <- sepBy (many $ satisfy isDigit) (char ' ')
  return (\(Rules r) -> do
    foldl1 (>>) (map ((($ Rules r) . (r M.!)) . read) ns) +++ foldl1 (>>) (map ((($ Rules r) . (r M.!)) . read) ms)
    return ())

character :: Parser (Rules -> Parser ())
character = do
  char '\"'
  c <- get
  char '\"'
  return $ const $ do
    char c
    return ()


first  (Rules rs) ms = (length . filter (== [""])) (map (map snd . parse ((rs M.! 0) (Rules rs))) ms)

second (Rules rs) ms = (length . filter (\ l -> l /= [] && last l == "")) (map (map snd . parse p0) ms)
  where p0  = do
          many1 $ (rs M.! 42) (Rules rs)
          p11
        p11 = do
          s <- look
          let l = length s
          choice $ map (\ i -> count i ((rs M.! 42) (Rules rs))>>count i ((rs M.! 31) (Rules rs)) >> return ()) [1..l]


--- ReadP Helpers ---

type Parser = ReadP

parse = readP_to_S

greedy :: Parser a -> Parser [a]
greedy p = do
  s <- look
  scan s
  where scan s = case parse p s of
                    []      -> return []
                    [(a,r)] -> do p; b <- scan r; return (a:b)
