module Main where
import Memory

to_int x = read x :: Int

main = do
  str <- getLine
  let input = to_int str
  putStrLn (show (find_value_larger_than input))

