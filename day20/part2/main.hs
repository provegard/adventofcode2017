module Main where
import Lib
import qualified Data.Sequence as Seq

main = do
  contents <- getContents
  let res = leftStanding (lines contents) 20
  print res
  print $ length res