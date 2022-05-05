data Orientation = East
                 | South
                 | West
                 | North
                 deriving (Enum, Show)

data Action = Move Orientation Int
            | Turn Int
            | Forward Int



fromFile path f = ( f . map (parse . splitAt 1) . lines) <$> readFile path

parse (a, i) = case a of
                 "E" -> Move East  $ read i
                 "S" -> Move South $ read i
                 "W" -> Move West  $ read i
                 "N" -> Move North $ read i
                 "R" -> Turn $          div (read i) 90
                 "L" -> Turn $ negate $ div (read i) 90
                 "F" -> Forward $ read i

first :: [Action] -> Int
first = (\ x -> (abs . fst) x + (abs . snd) x) . snd . foldl solveFirst (East, (0, 0))

solveFirst (o, p@(x, y)) action = case action of
                                    (Turn    i) -> (turn i o, p)
                                      where turn i o = toEnum $ mod (i + fromEnum o) 4

                                    (Move to i) -> move to i

                                    (Forward i) -> move  o i

                                    where move to i = case to of
                                                        East  -> (o, (x + i, y))
                                                        South -> (o, (x, y - i))
                                                        West  -> (o, (x - i, y))
                                                        North -> (o, (x, y + i))

second :: [Action] -> Int
second = (\ x -> (abs . fst) x + (abs . snd) x) . snd . foldl solveSecond ((10, 1), (0, 0))

solveSecond (wp@(a, b), p@(x, y)) action = case action of
                                             (Move to i) -> case to of
                                                              East  -> ((a + i, b    ), p)
                                                              South -> ((a,     b - i), p)
                                                              West  -> ((a - i, b    ), p)
                                                              North -> ((a,     b + i), p)

                                             (Forward i) -> ((a, b), (a * i + x, b * i + y))

                                             (Turn    i) -> (turn i wp, p)
                                               where turn i (a, b) | i <  0 = turn (i + 1) (- b,   a)
                                                                   | i >  0 = turn (i - 1) (  b, - a)
                                                                   | i == 0 = (a,b)

