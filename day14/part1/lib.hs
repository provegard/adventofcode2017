module Lib where
import Data.Foldable
import Hash (realHash)

toInt :: String -> Int
toInt x = read x :: Int

numBits :: Char -> Int
numBits '0' = 0
numBits '1' = 1
numBits '2' = 1
numBits '3' = 2
numBits '4' = 1
numBits '5' = 2
numBits '6' = 2
numBits '7' = 3
numBits '8' = 1
numBits '9' = 2
numBits 'a' = 2
numBits 'b' = 3
numBits 'c' = 2
numBits 'd' = 3
numBits 'e' = 3
numBits 'f' = 4

rowHashes :: String -> [String]
rowHashes input = let doHash i = realHash (input ++ "-" ++ show i)
                  in map doHash [0..127]

countBitsInHash :: String -> Int
countBitsInHash str = sum $ map numBits str

countBitsInGrid :: String -> Int
countBitsInGrid input = do
    let hashes = rowHashes input
    sum $ map countBitsInHash hashes