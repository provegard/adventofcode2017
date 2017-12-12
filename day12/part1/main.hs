module Main where
import Lib

main = do
  str <- getContents
  let size = groupSize 0 (lines str)
  print size
