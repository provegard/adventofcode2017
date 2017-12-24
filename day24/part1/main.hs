module Main where
import Lib

main = do
    contents <- getContents
    let strings = lines contents
    let (s, b) = findStrongest strings
    print s
    print b