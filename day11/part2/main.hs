module Main where
import Lib
import Data.List.Split

main = do
  str <- getLine
  let steps = splitOn "," str
  --let stepsReq = stepsRequired steps
  print (furthestDistance steps)
