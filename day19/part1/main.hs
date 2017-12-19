module Main where
import Lib
import qualified Data.Sequence as Seq

main = do
  contents <- getContents
  let c = collectAll (lines contents)
  putStrLn c
