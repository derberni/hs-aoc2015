{-# LANGUAGE TupleSections #-}

module Day6 (part1, part2) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import ParserUtil (decimal)
import Text.Parsec (ParseError, char, endOfLine, parse, sepBy, spaces, string, try, (<|>))
import Text.Parsec.String (Parser)

data Action = On | Off | Toggle deriving (Eq, Show)

actionOn :: Parser Action
actionOn = try $ string "turn on" *> return On

actionOff :: Parser Action
actionOff = try $ string "turn off" *> return Off

actionToggle :: Parser Action
actionToggle = try $ string "toggle" *> return Toggle

coord :: Parser (Int, Int)
coord = do
  e1 <- decimal
  _ <- char ','
  e2 <- decimal
  pure (e1, e2)

data Instruction = Instruction
  { action :: Action,
    from :: (Int, Int),
    to :: (Int, Int)
  }
  deriving (Eq, Show)

instruction :: Parser Instruction
instruction = do
  a <- actionOn <|> actionOff <|> actionToggle
  _ <- spaces
  c1 <- coord
  _ <- spaces
  _ <- string "through"
  _ <- spaces
  c2 <- coord
  pure $ Instruction a c1 c2

parseInstructions :: String -> Either ParseError [Instruction]
parseInstructions = parse (instruction `sepBy` endOfLine) ""

type State = Set.Set (Int, Int)

fromCoords :: (Int, Int) -> (Int, Int) -> State
fromCoords (x1, y1) (x2, y2) = Set.fromList ((,) <$> [x1 .. x2] <*> [y1 .. y2])

turnOn :: State -> (Int, Int) -> (Int, Int) -> State
turnOn old c1 c2 = Set.union old $ fromCoords c1 c2

turnOff :: State -> (Int, Int) -> (Int, Int) -> State
turnOff old c1 c2 = Set.filter (\c -> not $ Set.member c offCoords) old
  where
    offCoords = fromCoords c1 c2

toggle :: State -> (Int, Int) -> (Int, Int) -> State
toggle old c1 c2 = Set.filter (\c -> not $ Set.member c alreadyOn) $ Set.union old toggleCoords
  where
    alreadyOn = Set.intersection old toggleCoords
    toggleCoords = fromCoords c1 c2

step :: State -> Instruction -> State
step old (Instruction On c1 c2) = turnOn old c1 c2
step old (Instruction Off c1 c2) = turnOff old c1 c2
step old (Instruction Toggle c1 c2) = toggle old c1 c2

part1 :: String -> String
part1 s = case parseInstructions s of
  Left err -> show err
  Right instructions -> show . Set.size $ foldl step Set.empty instructions

type BrightnessState = Map.Map (Int, Int) Int

mapFromCoords :: Int -> (Int, Int) -> (Int, Int) -> BrightnessState
mapFromCoords v (x1, y1) (x2, y2) = Map.fromList $ (,v) <$> ((,) <$> [x1 .. x2] <*> [y1 .. y2])

brightnessOn :: BrightnessState -> (Int, Int) -> (Int, Int) -> BrightnessState
brightnessOn old c1 c2 = Map.unionWith (+) old $ mapFromCoords 1 c1 c2

-- unfortunately, Map.unionWith ignores the function if the left Map does not contain the key
-- therefore, add -1 and then cap to 0 afterwards
brightnessOff :: BrightnessState -> (Int, Int) -> (Int, Int) -> BrightnessState
brightnessOff old c1 c2 = Map.map (max 0) $ Map.unionWith (+) old $ mapFromCoords (-1) c1 c2

brightnessToggle :: BrightnessState -> (Int, Int) -> (Int, Int) -> BrightnessState
brightnessToggle old c1 c2 = Map.unionWith (+) old $ mapFromCoords 2 c1 c2

brightnessStep :: BrightnessState -> Instruction -> BrightnessState
brightnessStep old (Instruction On c1 c2) = brightnessOn old c1 c2
brightnessStep old (Instruction Off c1 c2) = brightnessOff old c1 c2
brightnessStep old (Instruction Toggle c1 c2) = brightnessToggle old c1 c2

part2 :: String -> String
part2 s = case parseInstructions s of
  Left err -> show err
  Right instructions -> show . Map.foldl' (+) 0 $ foldl brightnessStep Map.empty instructions