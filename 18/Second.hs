import Text.Parsec.String
import Text.Parsec
import Control.Monad (void)


data Expression = Addition Expression Expression
                | Multiplication Expression Expression
                | Parens Expression
                | Number Int deriving (Show, Eq)


parens :: Parser Expression
parens = between (char '(') (char ')') content
  where content = do
          start <- parens <|> number
          spaces
          es <- many (try plus <|> times)
          return $ Parens $ foldl (flip ($)) start es

expression :: Parser Expression
expression = do
  undefined 

--line :: Parser Expression
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

second = sum . map (calculate . change')

calculate :: Expression -> Int
calculate (Number i) = i
calculate (Parens e) = calculate e
calculate (Addition       e1 e2) = calculate e1 + calculate e2
calculate (Multiplication e1 e2) = calculate e1 * calculate e2


change' :: Expression -> Expression
change' e = if e == new then e else change' new
  where new = change e

change :: Expression -> Expression
change (Addition e  (Multiplication a b)) = Multiplication (change $ Addition e a) (change b)
change (Addition e1 e2)                   = Addition       (change e1)             (change e2)
change (Multiplication e1 e2)             = Multiplication (change e1)             (change e2)
change (Parens e)                         = Parens         (change e)
change (Number n)                         = Number n 
