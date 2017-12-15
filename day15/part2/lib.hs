module Lib where
import Data.Bits

generate :: Int -> Int -> [Int]
generate f n = do
    let next = (n * f) `mod` 2147483647
    next : generate f next

generateFiltered :: Int -> Int -> Int -> [Int]
generateFiltered m f n = [ x | x <- generate f n, x `mod` m == 0 ] 

valuesMatch :: Int -> Int -> Bool    
valuesMatch a b = do
    let lowA = (.&.) a 65535
    let lowB = (.&.) b 65535
    lowA == lowB

findMatchesFrom :: Int -> Int -> Int
findMatchesFrom a b = do
    let zipped = zip (generateFiltered 4 16807 a) (generateFiltered 8 48271 b)
    let toConsider = take 5000000 zipped
    foldl (\acc t -> acc + (if uncurry valuesMatch t then 1 else 0)) 0 toConsider
