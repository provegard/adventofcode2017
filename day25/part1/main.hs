module Main where
import Lib

main = do
    contents <- getContents
    let strings = lines contents
    let m = executeInput strings
    print $ checksum m