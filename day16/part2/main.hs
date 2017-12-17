module Main where
import Lib

toInt x = read x :: Int

main = do
  str <- getLine
  --let res = finalOrderMulti 1000000000 16 str
  let res = finalOrderMulti 10000 16 str
  putStrLn res
