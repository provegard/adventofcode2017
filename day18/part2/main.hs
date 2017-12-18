module Main where
import Lib

main = do
  contents <- getContents
  let instr = lines contents
  print $ sentByP1 instr
