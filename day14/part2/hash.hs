module Hash where
import Data.List
import qualified Data.Sequence as Seq
import Data.Foldable
import Data.Char
import Data.Bits
import Text.Printf

-- Recursively take _len_ numbers from _numbers_ starting at _pos_, wrapping around
-- if the end of the numbers sequence is encountered.
takeC :: Seq.Seq Int -> Int -> Int -> Seq.Seq Int
takeC numbers pos 0   = Seq.empty
takeC numbers pos len = numAtPos Seq.<| takeC numbers (pos + 1) (len - 1)
    where numAtPos = Seq.index numbers (pos `mod` length numbers)

-- Recursively update a sequence of numbers (_numbers_) by replacing them with numbers
-- from _span_ starting at position _pos_, wrapping around if the end of the numbers
-- sequence is encountered.
updateC :: Seq.Seq Int -> Int -> Seq.Seq Int -> Seq.Seq Int
updateC numbers pos span
    | Seq.length span == 0 = numbers
    | otherwise            = let newNums = Seq.update curPos (Seq.index span 0) numbers
                             in updateC newNums (pos + 1) (Seq.drop 1 span)
    where curPos = pos `mod` Seq.length numbers

-- Hashes numbers using a single length, depletes the lengths list.
hashNext :: Seq.Seq Int -> [Int] -> Int -> Int -> (Seq.Seq Int, Int, Int)
hashNext numbers [] curPos skipLen         = (numbers, curPos, skipLen)
hashNext numbers (len:rest) curPos skipLen = do
    let span = takeC numbers curPos len
    let newNums = updateC numbers curPos $ Seq.reverse span
    hashNext newNums rest (curPos + len + skipLen) (skipLen + 1) 

-- hashes the numbers in the _numbers_ sequence _count_ times
hashMany :: Int -> Seq.Seq Int -> [Int] -> Seq.Seq Int
hashMany count numbers lengths = fstOf3 $ hash' count numbers 0 0
    where
        fstOf3 (a,_,_) = a
        hash' 0 nums crp slen = (nums, crp, slen)
        hash' x nums crp slen = let (hashed, crp', slen') = hashNext nums lengths crp slen
                                in hash' (x - 1) hashed crp' slen'

makeDense :: Seq.Seq Int -> Seq.Seq Int
makeDense numbers = let chunks = Seq.chunksOf 16 numbers
                    in fmap dense chunks
    where dense s = Seq.index (Seq.scanr1 xor s) 0 -- scanr1 leaves the result as the head element

parseInput :: String -> [Int]
parseInput str = parse' str []
    where
        parse' [] accum     = accum ++ [17, 31, 73, 47, 23]
        parse' (x:xs) accum = parse' xs $ accum ++ [ord x]

realHash :: String -> String
realHash input = do
    let lengths = parseInput input
    let hashedNumbers = hashMany 64 (Seq.fromList [0..255]) lengths
    let denseNumbers = toList $ makeDense hashedNumbers
    concatMap toHex denseNumbers
    where toHex = printf "%02x"
        