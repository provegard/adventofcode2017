module Main where
import Lib

main = do
  str <- getContents
  let num = numberOfGroups $ lines str
  print num
