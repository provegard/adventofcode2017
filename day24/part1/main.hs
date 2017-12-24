module Main where
import Lib

main = do
    contents <- getContents
    let strings = lines contents
    let (s, _) = findStrongest strings
    print s