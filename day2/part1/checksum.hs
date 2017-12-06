module Checksum where

to_int :: String -> Int
to_int s = read s :: Int

line_diff :: [String] -> Int
line_diff row = do
  let numbers = map to_int row
  let max = maximum numbers
  let min = minimum numbers
  max - min
  

line_diffs rows = map line_diff rows

checksum table = sum (line_diffs table)
