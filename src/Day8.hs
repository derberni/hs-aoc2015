module Day8 (part1, part2) where

import Control.Monad (void)
import Data.Char (chr)
import Data.Foldable (Foldable (foldl'))
import Text.Parsec (ParseError, alphaNum, char, count, many, oneOf, parse, string, try, (<|>))
import Text.Parsec.String (Parser)

escapedChar :: Parser Char
escapedChar = do
  void $ char '\\'
  oneOf "\\\""

hexChar :: Parser Char
hexChar = do
  void $ string "\\x"
  hc <- count 2 alphaNum
  pure $ chr $ read $ "0x" ++ hc

parseString :: Parser String
parseString = do
  void $ char '"'
  s <- many (try escapedChar <|> try hexChar <|> alphaNum)
  void $ char '"'
  pure s

parseLine :: String -> Either ParseError String
parseLine = parse parseString ""

codeLength :: String -> Int
codeLength = length

charLength :: String -> Int
charLength s = case parseLine s of
  Left err -> error $ show err
  Right ps -> length ps

part1 :: String -> String
part1 s = show (totalCodeLength - totalCharLength)
  where
    totalCodeLength = foldl' (+) 0 $ map codeLength (lines s)
    totalCharLength = foldl' (+) 0 $ map charLength (lines s)

encodedLength :: String -> Int
encodedLength = length . show

part2 :: String -> String
part2 s = show (totalEncodedLength - totalCodeLength)
  where
    totalCodeLength = foldl' (+) 0 $ map codeLength (lines s)
    totalEncodedLength = foldl' (+) 0 $ map encodedLength (lines s)