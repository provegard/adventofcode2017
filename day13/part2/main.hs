module Main where
import Lib

main = do
  str <- getContents
  let s = getMinDelay $ lines str
  print s
