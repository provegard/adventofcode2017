module Main where
import Lib
import qualified Data.Sequence as Seq

main = do
  let (cp, s) = spinLock 50000000 328
  let s2 = Seq.dropWhileL ((/=) 0) s
  let nextVal = Seq.index s2 1
  putStrLn (show nextVal)
