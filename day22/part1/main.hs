module Main where
import Lib
import qualified Data.Sequence as Seq

main = do
  contents <- getContents
  let world = parseWorld (lines contents)
  let (World _ _ _ infected) = burst world 10000
  print infected