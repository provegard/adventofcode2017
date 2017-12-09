module Lib where
import Data.Maybe
import Data.List
import Data.Stack

newtype Group = Group { score :: Int } deriving (Show, Eq)

data Memory = Memory
              { stck :: Stack Group
              , groups :: [Group]
              , garbageCount :: Int
              } deriving (Show)

dropGarbage0 :: String -> (String, Int)
dropGarbage0 "" = ("", 0)
dropGarbage0 stream = case head stream of
    '!' -> dropGarbage0 $ drop 2 stream
    '>' -> (tail stream, 0)
    _   -> do
        let rest = dropGarbage0 $ tail stream
        (fst rest, 1 + snd rest)
dropGarbage :: String -> (String, Int)
dropGarbage "" = ("", 0)
dropGarbage stream = case head stream of
    '<' -> dropGarbage0 $ tail stream
    x   -> error ("unexpected garbage: " ++ [x])

newGroup stack = case stackPeek stack of
    Just (Group s) -> stackPush stack $ Group (s + 1)
    Nothing -> stackPush stack $ Group 1

findGroupsRec :: Memory -> String -> Memory
findGroupsRec mem "" = mem
findGroupsRec (Memory stack done gc) stream = case head stream of
    '{' -> findGroupsRec (Memory (newGroup stack) done gc) (tail stream)
    '}' -> case stackPop stack of
        Just (s, g) -> findGroupsRec (Memory s (g : done) gc) (tail stream)
        Nothing -> error "empty stack"
    ',' -> findGroupsRec (Memory stack done gc) (tail stream)
    _   -> do
        let gbg = dropGarbage stream
        findGroupsRec (Memory stack done (snd gbg + gc)) (fst gbg)
        
countGarbage :: String -> Int
countGarbage stream = do
    let mem = findGroupsRec (Memory stackNew [] 0) stream
    garbageCount mem