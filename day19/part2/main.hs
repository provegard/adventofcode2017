module Main where
import Lib
import qualified Data.Sequence as Seq

main = do
  contents <- getContents
  let cnt = countSteps (lines contents)
  print cnt
