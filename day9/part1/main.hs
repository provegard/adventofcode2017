module Main where
import Lib

main = do
  stream <- getLine
  let score = findScore stream
  print score
