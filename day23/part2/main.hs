module Main where
import Lib

main = do
  contents <- getContents
  let instr = lines contents
  print $ run instr
