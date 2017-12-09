module Main where
import Lib

main = do
  stream <- getLine
  let gc = countGarbage stream
  putStrLn (show gc)
