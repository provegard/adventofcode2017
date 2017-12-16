module Main where
import Lib

toInt x = read x :: Int

main = do
  str <- getLine
  let res = finalOrder 16 str
  putStrLn res
