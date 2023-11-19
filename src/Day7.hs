module Day7 (part1, part2) where

import Control.Monad (void)
import Data.Bits (complement, shiftL, shiftR, (.&.), (.|.))
import qualified Data.Map as Map
import Data.Word (Word16)
import Text.Parsec (digit, endOfLine, lower, many1, parse, sepBy, space, spaces, string, try, (<|>))
import Text.Parsec.String (Parser)

int :: Parser Int
int = read <$> many1 digit

literal :: Parser Signal
literal = Literal . read <$> many1 digit

wire :: Parser Signal
wire = Wire <$> many1 lower

signal :: Parser Signal
signal = wire <|> literal

gateEnd :: Parser ()
gateEnd = do
  spaces
  void $ string "->"
  spaces

idGate :: Parser Gate
idGate = do
  s <- signal
  gateEnd
  pure $ IDGate s

notGate :: Parser Gate
notGate = do
  _ <- string "NOT"
  _ <- space
  w <- wire
  gateEnd
  pure $ NotGate w

lshiftGate :: Parser Gate
lshiftGate = do
  w <- wire
  _ <- space
  _ <- string "LSHIFT"
  _ <- space
  d <- int
  gateEnd
  pure $ LShiftGate w d

rshiftGate :: Parser Gate
rshiftGate = do
  w <- wire
  _ <- space
  _ <- string "RSHIFT"
  _ <- space
  d <- int
  gateEnd
  pure $ RShiftGate w d

andGate :: Parser Gate
andGate = do
  s1 <- signal
  _ <- space
  _ <- string "AND"
  _ <- space
  s2 <- signal
  gateEnd
  pure $ AndGate s1 s2

orGate :: Parser Gate
orGate = do
  s1 <- signal
  _ <- space
  _ <- string "OR"
  _ <- space
  s2 <- signal
  gateEnd
  pure $ OrGate s1 s2

-- parseLine could be improved by avoiding all the lookaheads with 'try' by left factoring
parseLine :: Parser (String, Gate)
parseLine = do
  g <- try idGate <|> try notGate <|> try lshiftGate <|> try rshiftGate <|> try andGate <|> orGate
  (Wire w) <- wire
  pure (w, g)

data Signal = Literal Word16 | Wire String deriving (Show)

data Gate
  = IDGate Signal
  | NotGate Signal
  | LShiftGate Signal Int
  | RShiftGate Signal Int
  | AndGate Signal Signal
  | OrGate Signal Signal
  deriving (Show)

type GateMap = Map.Map String Gate

updateMap :: GateMap -> String -> Word16 -> GateMap
updateMap m w v = Map.insert w (IDGate (Literal v)) m

-- resolveGate does all the work
-- the function resolves the signal a gate outputs recursively and memoizes already resolved gates by
-- updating the GateMap after each successful resolve
resolveGate :: GateMap -> Gate -> (Word16, GateMap)
resolveGate m (IDGate (Wire w)) = (v, updateMap nm w v) where (v, nm) = resolveGate m $ m Map.! w
resolveGate m (IDGate (Literal l)) = (l, m)
resolveGate m (NotGate (Wire w)) = (complement v, updateMap nm w v) where (v, nm) = resolveGate m $ m Map.! w
resolveGate m (LShiftGate (Wire w) s) = (shiftL v s, updateMap nm w v) where (v, nm) = resolveGate m $ m Map.! w
resolveGate m (RShiftGate (Wire w) s) = (shiftR v s, updateMap nm w v) where (v, nm) = resolveGate m $ m Map.! w
resolveGate m (AndGate (Wire w1) (Wire w2)) = (v1 .&. v2, updateMap nm2 w2 v2)
  where
    (v1, nm1) = resolveGate m $ m Map.! w1
    (v2, nm2) = resolveGate nm1 $ nm1 Map.! w2
resolveGate m (AndGate (Literal l) (Wire w2)) = (l .&. v2, updateMap nm2 w2 v2) where (v2, nm2) = resolveGate m $ m Map.! w2
resolveGate m (OrGate (Wire w1) (Wire w2)) = (v1 .|. v2, updateMap nm2 w2 v2)
  where
    (v1, nm1) = resolveGate m $ m Map.! w1
    (v2, nm2) = resolveGate nm1 $ nm1 Map.! w2
resolveGate _ _ = error "unmatched case"

part1 :: String -> String
part1 s = case parse (parseLine `sepBy` endOfLine) "" s of
  Left err -> show err
  Right g -> show . fst . resolveGate m $ m Map.! "a"
    where
      m = Map.fromList g :: GateMap

part2 :: String -> String
part2 s = case parse (parseLine `sepBy` endOfLine) "" s of
  Left err -> show err
  Right g -> show . fst . resolveGate m2 $ m2 Map.! "a"
    where
      m = Map.fromList g :: GateMap
      resolvedA = fst . resolveGate m $ m Map.! "a"
      m2 = updateMap m "b" resolvedA