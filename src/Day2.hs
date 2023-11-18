module Day2 (day2part1, day2part2) where

import ParserUtil (decimal)
import Text.Parsec (ParseError, endOfLine, parse, string)
import Text.Parsec.Combinator (sepBy)
import Text.Parsec.String (Parser)

data Box = Box
  { width :: Int,
    height :: Int,
    length :: Int
  }
  deriving (Show, Eq)

box :: Parser Box
box = do
  x <- decimal
  _ <- string "x"
  y <- decimal
  _ <- string "x"
  z <- decimal
  return $ Box x y z

parseBoxes :: String -> Either ParseError [Box]
parseBoxes = parse (box `sepBy` endOfLine) ""

smallestPerimeter :: Box -> Int
smallestPerimeter (Box w h l) = 2 * min (w + h) (min (w + l) (h + l))

volume :: Box -> Int
volume (Box w h l) = w * h * l

paper :: Box -> Int
paper (Box w h l) = 2 * w * h + 2 * l * w + 2 * h * l + min (w * h) (min (w * l) (h * l))

ribbon :: Box -> Int
ribbon b = smallestPerimeter b + volume b

day2part1 :: String -> String
day2part1 s =
  case parseBoxes s of
    Left err -> show err
    Right boxes -> show (sum (paper <$> boxes))

day2part2 :: String -> String
day2part2 s =
  case parseBoxes s of
    Left err -> show err
    Right boxes -> show (sum (ribbon <$> boxes))