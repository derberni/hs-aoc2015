module Main (main) where

import Day1 (day1part1, day1part2)
import Day2 (day2part1, day2part2)
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import System.Environment (getArgs)

dispatch :: [(String, String -> String)]
dispatch =
  [ ("day1part1", day1part1),
    ("day1part2", day1part2),
    ("day2part1", day2part1),
    ("day2part2", day2part2),
    ("day3part1", Day3.part1),
    ("day3part2", Day3.part2),
    ("day4part1", Day4.part1),
    ("day4part2", Day4.part2),
    ("day5part1", Day5.part1),
    ("day5part2", Day5.part2),
    ("day6part1", Day6.part1),
    ("day6part2", Day6.part2),
    ("day7part1", Day7.part1),
    ("day7part2", Day7.part2)
  ]

main :: IO ()
main = do
  [task, dataFile] <- getArgs
  let (Just action) = lookup task dispatch
  result <- action <$> readFile dataFile
  putStrLn result
