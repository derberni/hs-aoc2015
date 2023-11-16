module Main (main) where

import Day1 (day1part1, day1part2)
import Day2 (day2part1, day2part2)
import System.Environment (getArgs)

dispatch :: [(String, String -> String)]
dispatch =
  [ ("day1part1", day1part1),
    ("day1part2", day1part2),
    ("day2part1", day2part1),
    ("day2part2", day2part2)
  ]

main :: IO ()
main = do
  [task, dataFile] <- getArgs
  let (Just action) = lookup task dispatch
  result <- action <$> readFile dataFile
  putStrLn result
