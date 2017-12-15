module Main where
import Lib

main = do
  str <- getLine
  let s = countRegions str
  print s
