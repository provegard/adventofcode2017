module Main where
import Lib

toInt x = read x :: Int

main = do
  str <- getLine
  let (a:b:_) = words str
  let res = findMatchesFrom (toInt a) (toInt b)
  print res
