module Lib where
import Data.List
import qualified Data.Sequence as Seq
import Data.Foldable
import Data.List.Split
import Data.Char
import Data.Bits
import Text.Printf

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

-- hashes using one length, depletes the lengths list
hashNext :: Seq.Seq Int -> [Int] -> Int -> Int -> (Seq.Seq Int, Int, Int)
hashNext numbers [] curPos skipLen         = (numbers, curPos, skipLen)
hashNext numbers (len:rest) curPos skipLen = do
    let span = takeC numbers curPos len
    let rev = Seq.reverse span
    let newNums = updateC numbers curPos rev
    hashNext newNums rest (curPos + len + skipLen) (skipLen + 1) 

-- hashes many times
hashMany :: Int -> Seq.Seq Int -> [Int] -> Int -> Int -> Seq.Seq Int
hashMany countLeft numbers lengths curPos skipLen = do
    let (hashed, _, _) = hash' countLeft numbers curPos skipLen
    hashed
    where
        hash' 0 nums crp slen = (nums, crp, slen)
        hash' x nums crp slen = do
            let (hashed, crp', slen') = hashNext nums lengths crp slen
            hash' (x - 1) hashed crp' slen'

dense :: Seq.Seq Int -> Int
dense s = foldl xor (Seq.index s 0) (Seq.drop 1 s)

densify :: Seq.Seq Int -> Seq.Seq Int
densify numbers = do
    let chunks = Seq.chunksOf 16 numbers
    Seq.mapWithIndex (\_ c -> dense c) chunks

parseInput :: String -> [Int]
parseInput str = parse' str []
    where
        parse' :: String -> [Int] -> [Int]
        parse' [] accum     = accum ++ [17, 31, 73, 47, 23]
        parse' (x:xs) accum = parse' xs (accum ++ [ord x])

realHash :: String -> String
realHash input = do
    let lengths = parseInput input
    let hashed = hashMany 64 (Seq.fromList [0..255]) lengths 0 0
    let d = toList $ densify hashed
    let hex = map toHex d
    concat hex
    where toHex = printf "%02x"
        