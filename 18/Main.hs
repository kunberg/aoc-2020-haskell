import Text.Parsec.String
import Text.Parsec
import Control.Monad (void)


data Expression = Addition Expression Expression
                | Multiplication Expression Expression
                | Number Int deriving Show


parens :: Parser Expression
parens = between (char '(') (char ')') content
  where content = do
          start <- parens <|> number
          spaces
          es <- many (try plus <|> times)
          return $ foldl (flip ($)) start es
  
line :: Parser Expression
line = do
  start <- parens <|> number
  spaces
  es <- manyTill (try plus <|> times) $ void endOfLine <|> eof 
  return $ foldl (flip ($)) start es
  
number :: Parser Expression
number = do
  i <- digit
  return $ Number $ read [i]

plus :: Parser (Expression -> Expression)
plus = do
  spaces
  char '+'
  spaces
  Addition <$> (parens <|> number) 

times :: Parser (Expression -> Expression)
times = do
  spaces
  char '*'
  spaces
  Multiplication <$> (parens <|> number) 

fromFile path f = fmap f <$> parseFromFile (many line) path

first = sum . map calculate

calculate :: Expression -> Int
calculate (Number i) = i
calculate (Addition       e1 e2) = calculate e1 + calculate e2
calculate (Multiplication e1 e2) = calculate e1 * calculate e2
