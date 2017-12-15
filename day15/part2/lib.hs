module Lib where
import Data.Bits

generate :: Int -> Int -> [Int]
generate f n = let next = (n * f) `mod` 2147483647
               in next : generate f next

generateFiltered :: Int -> Int -> Int -> [Int]
generateFiltered m f n = [ x | x <- generate f n, x `mod` m == 0 ] 

valuesMatch :: Int -> Int -> Bool    
valuesMatch a b = let low16 = (.&.) (65535 :: Int)
                  in low16 a == low16 b

findMatchesFrom :: Int -> Int -> Int
findMatchesFrom a b = do
    let zipped = zip (generateFiltered 4 16807 a) (generateFiltered 8 48271 b)
    let toConsider = take 5000000 zipped
    foldl (\acc t -> acc + (if uncurry valuesMatch t then 1 else 0)) 0 toConsider
