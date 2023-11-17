module Day5 (part1, part2) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.List (isInfixOf)

forbidden :: [BS.ByteString]
forbidden = map C.pack ["ab", "cd", "pq", "xy"]

vowels :: [Char]
vowels = "aeiou"

contains3Vowels :: BS.ByteString -> Bool
contains3Vowels bs = 3 <= BS.length (C.filter (`elem` vowels) bs)

containsForbidden :: BS.ByteString -> Bool
containsForbidden bs = any (`BS.isInfixOf` bs) forbidden

containsDoubleChar :: BS.ByteString -> Bool
containsDoubleChar bs = any (\g -> 1 < BS.length g) groups
  where
    groups = BS.group bs

niceString :: BS.ByteString -> Bool
niceString bs
  | containsForbidden bs = False
  | not $ containsDoubleChar bs = False
  | not $ contains3Vowels bs = False
  | otherwise = True

part1 :: String -> String
part1 s = show . length $ filter niceString bsLines
  where
    bsLines = map C.pack $ lines s

containsPair :: String -> Bool
containsPair (x : y : xs) = [x, y] `isInfixOf` xs || containsPair (y : xs)
containsPair _ = False

containsRepeatLetter :: String -> Bool
containsRepeatLetter (x : y : z : xs) = (x == z) || containsRepeatLetter (y : z : xs)
containsRepeatLetter _ = False

niceString2 :: String -> Bool
niceString2 s
  | not $ containsPair s = False
  | not $ containsRepeatLetter s = False
  | otherwise = True

part2 :: String -> String
part2 s = show . length $ filter niceString2 ps
  where
    ps = lines s