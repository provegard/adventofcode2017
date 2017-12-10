module Main where
import Lib
import Data.List.Split

toInt x = read x :: Int

main = do
  lenStr <- getLine
  let lengths = map toInt $ splitOn "," lenStr
  let ver = verHash [0..255] lengths
  print ver
