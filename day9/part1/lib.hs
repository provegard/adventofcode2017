module Lib where

data Memory = Memory { currentGroupScore :: Int, accumScore:: Int }

dropGarbage :: String -> String
dropGarbage "" = ""
dropGarbage ('<':xs)   = dropGarbage xs
dropGarbage ('!':_:xs) = dropGarbage xs
dropGarbage ('>':xs)   = xs
dropGarbage (_:xs)     = dropGarbage xs

process :: Memory -> String -> Memory
process mem ""                       = mem
process (Memory cur acc) ('{':xs)    = process (Memory (cur + 1) acc) xs
process mem (',':xs)                 = process mem xs
process (Memory cur acc) ('}':xs)    = process (Memory (cur - 1) (acc + cur)) xs
process mem stream                   = process mem $ dropGarbage stream

findScore :: String -> Int
findScore stream = accumScore $ process emptyMem stream 
    where emptyMem = Memory 0 0
