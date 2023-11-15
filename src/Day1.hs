module Day1
  ( day1part1,
    day1part2,
  )
where

parenthesisToInt :: Char -> Int
parenthesisToInt '(' = 1
parenthesisToInt ')' = -1
parenthesisToInt _ = 0

foldParenthesis :: Int -> Char -> Int
foldParenthesis i c = i + parenthesisToInt c

day1part1 :: String -> String
day1part1 = show . foldl foldParenthesis 0

day1part2 :: String -> String
day1part2 s = show . length . takeWhile (/= -1) $ scanl foldParenthesis 0 s