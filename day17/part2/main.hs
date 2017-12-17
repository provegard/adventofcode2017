module Main where
import Lib
import qualified Data.Sequence as Seq

main = do
  let (_,buf) = spinLock 50000000 328
  print $ valueAfter0 buf
