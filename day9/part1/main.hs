module Main where
import Lib

main = do
  stream <- getLine
  let foundGroups = findGroups stream
  let scores = map score foundGroups
  let totalScore = sum scores
  putStrLn (show totalScore)
