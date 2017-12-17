module Main where
import Lib
import qualified Data.Sequence as Seq

main = do
  let st = spinLock 2017 328
  print $ nextVal st
