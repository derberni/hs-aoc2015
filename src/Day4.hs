module Day4 (part1, part2) where

import Crypto.Hash.MD5 (hash)
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Text.Parsec (anyChar, many1, parse)
import Text.Parsec.String (Parser)

parseSecret :: Parser String
parseSecret = many1 anyChar

assemble :: String -> Int -> BS.ByteString
assemble secret n = C.pack (secret ++ show n)

startsWith0 :: BS.ByteString -> Bool
startsWith0 bs = case BS.unpack $ BS.take 3 bs of
  0 : 0 : b : _ -> (b `shiftR` 4) == 0
  _ -> False

startsWithMore0 :: BS.ByteString -> Bool
startsWithMore0 bs = case BS.unpack $ BS.take 3 bs of
  0 : 0 : 0 : _ -> True
  _ -> False

enumHash :: String -> Int -> (Int, BS.ByteString)
enumHash s i = (i, hash $ assemble s i)

part1 :: String -> String
part1 s = case parse parseSecret "" s of
  Left err -> show err
  Right secret -> show . head . dropWhile (not . startsWith0 . snd) $ map (enumHash secret) $ iterate (+ 1) 1

part2 :: String -> String
part2 s = case parse parseSecret "" s of
  Left err -> show err
  Right secret -> show . head . dropWhile (not . startsWithMore0 . snd) $ map (enumHash secret) $ iterate (+ 1) 1