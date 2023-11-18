module ParserUtil (decimal) where

import Data.Char (digitToInt)
import Text.Parsec (digit, many1)
import Text.Parsec.String (Parser)

atoi :: [Char] -> Int
atoi = foldl f 0
  where
    f s x = 10 * s + digitToInt x

decimal :: Parser Int
decimal = atoi <$> many1 digit