module Checksum where
import Data.List

to_int :: String -> Int
to_int s = read s :: Int

pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

div_evenly_or_zero tup = do
  let a = fst tup
  let b = snd tup
  let bigger = max a b
  let smaller = min a b
  if bigger `mod` smaller == 0 then bigger `div` smaller else 0

line_value :: [String] -> Int
line_value row = do
  let numbers = map to_int row
  let all_pairs = pairs numbers
  let quotients = map div_evenly_or_zero all_pairs
  (sum quotients)
  

line_values rows = map line_value rows

checksum table = sum (line_values table)
