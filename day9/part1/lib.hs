module Lib where
import Data.Stack

newtype Group = Group { score :: Int } deriving (Show, Eq)

data Memory = Memory
              { stck :: Stack Group
              , groups :: [Group]
              } deriving (Show)

dropGarbage :: String -> String
dropGarbage "" = ""
dropGarbage ('<':xs)   = dropGarbage xs
dropGarbage ('!':_:xs) = dropGarbage xs
dropGarbage ('>':xs)   = xs
dropGarbage (_:xs)     = dropGarbage xs

newGroup stack = case stackPeek stack of
    Just (Group s) -> stackPush stack $ Group $ s + 1
    Nothing -> stackPush stack $ Group 1

findGroupsRec :: Memory -> String -> Memory
findGroupsRec mem ""                       = mem
findGroupsRec (Memory stack done) ('{':xs) = findGroupsRec (Memory (newGroup stack) done) xs
findGroupsRec mem (',':xs)                 = findGroupsRec mem xs
findGroupsRec (Memory stack done) ('}':xs) = case stackPop stack of
    Just (s, g) -> findGroupsRec (Memory s (g : done)) xs
    Nothing -> error "empty stack"
findGroupsRec mem stream                   = findGroupsRec mem $ dropGarbage stream

findGroups :: String -> [Group]
findGroups stream = groups $ findGroupsRec emptyMem stream 
    where emptyMem = Memory stackNew []
