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

-- dropGarbage0 parses known garbage ('<' has already been processed)
dropGarbage0 :: String -> (String, Int)
dropGarbage0 ""         = ("", 0)
dropGarbage0 ('!':_:xs) = dropGarbage0 xs
dropGarbage0 ('>':xs)   = (xs, 0)
dropGarbage0 (_:xs)     = (rest, 1 + gc) where (rest, gc) = dropGarbage0 xs

-- dropGarbage is the starting point and accepts only '<'
dropGarbage :: String -> (String, Int)
dropGarbage ('<':xs) = dropGarbage0 xs
dropGarbage (x:xs)   = error ("unexpected garbage: " ++ [x])

newGroup stack = stackPush stack $ Group (maybe 1 (\ g -> score g + 1) (stackPeek stack))

findGroupsRec :: Memory -> String -> Memory
findGroupsRec mem "" = mem
findGroupsRec (Memory stack done gc) ('{':xs) = findGroupsRec (Memory (newGroup stack) done gc) xs
findGroupsRec mem (',':xs) = findGroupsRec mem xs
findGroupsRec (Memory stack done gc) ('}':xs) = case stackPop stack of
    Just (s, g) -> findGroupsRec (Memory s (g : done) gc) xs
    Nothing -> error "empty stack"
findGroupsRec (Memory stack done gc) stream = findGroupsRec (Memory stack done (skippedGc + gc)) rest
    where (rest, skippedGc) = dropGarbage stream
        
countGarbage :: String -> Int
countGarbage stream = garbageCount $ findGroupsRec emptyMem stream
    where emptyMem = Memory stackNew [] 0
