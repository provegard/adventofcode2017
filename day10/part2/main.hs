module Main where
import Lib
import Data.List.Split

main = do
  input <- getLine
  putStrLn $ realHash input
