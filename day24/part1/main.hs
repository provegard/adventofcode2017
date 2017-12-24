module Main where
import Lib

main = do
  contents <- getContents
  let strings = lines contents
  print $ findStrongest strings
