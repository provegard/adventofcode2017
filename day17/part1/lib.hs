module Lib where
import Data.Char
import qualified Data.Sequence as Seq

doStep :: (Int, Seq.Seq Int) -> Int -> Int -> Int -> (Int, Seq.Seq Int)
doStep state 0 _ _ = state
doStep (cp, s) stepsLeft stepSize insertionValue = do
    let newPos = 1 + ((cp + stepSize) `mod` Seq.length s)
    let newS = Seq.insertAt newPos insertionValue s
    doStep (newPos, newS) (stepsLeft - 1) stepSize (insertionValue + 1)


spinLock :: Int -> Int -> (Int, Seq.Seq Int)
spinLock iterationCount stepSize = do
    let initial = Seq.fromList [0]
    doStep (0, initial) iterationCount stepSize 1