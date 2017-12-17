{-# LANGUAGE BangPatterns #-}
module Lib where
import Data.Char
import qualified Data.Sequence as Seq

type Buffer = Seq.Seq Int
type State = (Int, Buffer)

doStep :: State -> Int -> Int -> Int -> State
doStep buf 0 _ _ = buf
doStep (cp, !buf) stepsLeft stepSize insertionValue = do
    let newPos = 1 + ((cp + stepSize) `mod` Seq.length buf)
    let newBuf = Seq.insertAt newPos insertionValue buf
    doStep (newPos, newBuf) (stepsLeft - 1) stepSize (insertionValue + 1)

spinLock :: Int -> Int -> State
spinLock iterationCount stepSize = do
    let initial = Seq.fromList [0]
    doStep (0, initial) iterationCount stepSize 1

valueAfter0 :: Buffer -> Int
valueAfter0 buf = do
    let rest = Seq.dropWhileL (0 /=) buf
    Seq.index rest 1
