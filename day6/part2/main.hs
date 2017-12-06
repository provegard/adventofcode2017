module Main where
import Redist

to_int x = read x :: Int

main = do
  line <- getLine
  let banks = map to_int (words line)
  putStrLn (show $ loop_size banks)

