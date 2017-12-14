module Main where
import Lib

main = do
  str <- getLine
  let s = countBitsInGrid str
  print s
