module Lib where
import Data.List
import qualified Data.Sequence as Seq
import Data.Foldable

takeC :: Seq.Seq Int -> Int -> Int -> Seq.Seq Int
takeC numbers pos 0   = Seq.empty
takeC numbers pos len = curElem Seq.<| takeC numbers (pos + 1) (len - 1)
    where curElem = Seq.index numbers (pos `mod` length numbers)

updateC :: Seq.Seq Int -> Int -> Seq.Seq Int -> Seq.Seq Int
updateC numbers pos span = do
    if Seq.length span == 0 then
        numbers
    else do
        let newNums = Seq.update curPos (Seq.index span 0) numbers
        updateC newNums (pos + 1) (Seq.drop 1 span)
    where curPos = pos `mod` Seq.length numbers

hashNext :: Seq.Seq Int -> [Int] -> Int -> Int -> Seq.Seq Int
hashNext numbers [] curPos skipLen         = numbers
hashNext numbers (len:rest) curPos skipLen = do
    let span = takeC numbers curPos len
    let rev = Seq.reverse span
    let newNums = updateC numbers curPos rev
    hashNext newNums rest (curPos + len + skipLen) (skipLen + 1) 

hash :: [Int] -> [Int] -> [Int]
hash numbers lengths = toList $ hashNext (Seq.fromList numbers) lengths 0 0

verHash :: [Int] -> [Int] -> Int
verHash numbers lengths = do
    let (x1:x2:_) = hash numbers lengths
    x1 * x2
