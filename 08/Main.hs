import           Data.Foldable (find)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq

data Operation = Acc Int
               | Jmp Int
               | Nop Int
               deriving Show

fromFile path f = ( f . fmap parse . Seq.fromList . lines) <$> readFile path

parse line = case o of
               "acc " -> Acc $ readInt i
               "jmp " -> Jmp $ readInt i
               "nop " -> Nop $ readInt i
  where (o, i)    = splitAt 4 line
        readInt i = case sign of
                      "+" -> read number
                      "-" -> negate $ read number
          where (sign, number) = splitAt 1 i

first  = snd . solve 0 0 . fmap Just
second = find ((True ==) .fst)  . fmap (solve 0 0) . (fmap . fmap) Just . possiblePrograms

possiblePrograms :: Seq Operation -> Seq (Seq Operation)
possiblePrograms prog = Seq.mapWithIndex (\ i _ -> change i prog) prog

change :: Int -> Seq Operation -> Seq Operation
change ind prog = Seq.update ind op prog
  where op = case Seq.index prog ind  of
               Acc i -> Acc i
               Jmp i -> Nop i
               Nop i -> Jmp i


solve :: Int -> Int -> Seq (Maybe Operation) -> (Bool, Int)
solve line acc prog = if line == (length prog) then (True, acc) else
                      case Seq.index prog line of
                        Nothing -> (False, acc)
                        Just op -> case op of
                                     Acc i -> solve (line + 1) (acc + i) newProg
                                     Jmp i -> solve (line + i)  acc      newProg
                                     Nop _ -> solve (line + 1)  acc      newProg
                          where newProg = Seq.update line Nothing prog
