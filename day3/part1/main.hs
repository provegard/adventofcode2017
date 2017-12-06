module Main where
import Memory

to_int x = read x :: Int

main = do
  str <- getLine
  let square_no = to_int str
  putStrLn (show (carry_distance square_no))

