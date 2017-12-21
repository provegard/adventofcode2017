module Main where
import Lib
import qualified Data.Sequence as Seq

main = do
  contents <- getContents
  let res = run (lines contents) 5
  print res