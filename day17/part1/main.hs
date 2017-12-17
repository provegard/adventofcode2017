module Main where
import Lib
import qualified Data.Sequence as Seq

main = do
  let (cp, s) = spinLock 2017 328
  let nextVal = Seq.index s (cp + 1)
  putStrLn (show nextVal)
