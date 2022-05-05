import           Control.Monad (void, liftM, replicateM, replicateM_)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (isJust)
import           Text.Parsec
import           Text.Parsec.String
import Maybes (isNothing, rightToMaybe)
import Data.Foldable (fold)
import Data.Either (isLeft)


newtype Rules = Rules {getRules :: Map Int (Rules -> Parser ())}

type Message = String

fromFile :: FilePath -> (Rules -> [Message] -> a) -> IO (Either ParseError a)
fromFile path f = fmap (uncurry f) <$> parseFromFile input path

input :: Parser (Rules, [Message])
input = do
  rules <- manyTill rule newline
  messages <- endBy (many $ noneOf "\n") newline
  return (Rules $ M.fromList rules, messages)

rule :: Parser (Int, Rules -> Parser ())
rule = do
  n <- (read :: String -> Int) <$> many1 digit
  string ": "
  r <- character <|> try two <|> try one
  newline
  return (n,r)

one :: Parser (Rules -> Parser ())
one = do
  ns <- sepBy (read <$> many digit) (char ' ')
  return (\(Rules r) -> do
    foldMap (($ Rules r) . (r M.!)) ns
    return ())

two :: Parser (Rules -> Parser ())
two = do
  ns <- endBy ((read :: String -> Int) <$> many digit) (char ' ')
  string "| "
  ms <- sepBy ((read :: String -> Int) <$> many digit) (char ' ')
  return (\(Rules r) -> do
    try (foldMap (($ Rules r) . (r M.!)) ns) <|> foldMap (($ Rules r) . (r M.!)) ms
    return ())

character :: Parser (Rules -> Parser ())
character = do
  char '\"'
  c <- anyChar
  char '\"'
  return $ const $ do
    char c
    return ()

first  (Rules rs) = length . filter (== Right ()) . map (parse ((rs M.! 0) (Rules rs) <> void eof) "")



second (Rules rs) = first $ Rules $ M.insert 0 special0 rs
  where special0 rules = do
          special8 rules
          test rules
          return ()
        test rules = do
          try (special11 rules) <> (special8 rules <> test rules)
          return ()
        special8 (Rules rs) = do
          (rs M.! 42) (Rules rs)
          return ()
        special11 (Rules rs) = do
          count <- many1 ((rs M.! 42) (Rules rs))
          replicateM_ (length count) ((rs M.! 31) (Rules rs))


special8 :: Message -> [Parser ()]
special8 m = ps
  where ps = takeWhile (\a -> isLeft $ parse a "" m) pss
        pss = undefined :: [Parser ()]
