module Main where
import Lib
import qualified Data.Sequence as Seq

main = do
  contents <- getContents
  let res = longRun (lines contents) 1100
  print res