module Main where
import Lib

main = do
  str <- getContents
  let s = getSeverity $ lines str
  print s
