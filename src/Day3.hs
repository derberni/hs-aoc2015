module Day3 (part1, part2) where

import qualified Data.Set as Set
import Text.Parsec (ParseError, anyChar, many1, parse)
import Text.Parsec.String (Parser)

move :: Parser (Int, Int)
move = do
  c <- anyChar
  case c of
    '<' -> return (-1, 0)
    '^' -> return (0, 1)
    'v' -> return (0, -1)
    '>' -> return (1, 0)
    _ -> return (0, 0)

parseMoves :: String -> Either ParseError [(Int, Int)]
parseMoves = parse (many1 move) ""

data State = State
  { position :: (Int, Int),
    visited :: Set.Set (Int, Int)
  }
  deriving (Eq, Show)

startState :: State
startState = State sp (Set.fromList [sp])
  where
    sp = (0, 0)

addT :: (Int, Int) -> (Int, Int) -> (Int, Int)
addT (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

advance :: State -> (Int, Int) -> State
advance (State p v) m = State np (Set.insert np v)
  where
    np = p `addT` m

advanceTwo :: (State, State) -> (Int, Int) -> (State, State)
advanceTwo (s1, s2) m = (s2, advance s1 m)

part1 :: String -> String
part1 s = case parseMoves s of
  Left err -> show err
  Right m -> show $ Set.size . visited $ foldl advance startState m

part2 :: String -> String
part2 s = case parseMoves s of
  Left err -> show err
  Right m -> show $ Set.size $ Set.union (visited s1) (visited s2)
    where
      (s1, s2) = foldl advanceTwo (startState, startState) m