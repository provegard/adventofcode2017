module Lib where
import Data.Maybe
import Data.List

newtype Group = Group { score :: Int } deriving (Show, Eq)

newtype Stack = Stack [Group] deriving (Show)

data Memory = Memory
              { stck :: Stack
              , groups :: [Group]
              } deriving (Show)

stackNew = Stack []
stackPush (Stack gs) g = Stack $ g : gs
stackPeek (Stack []) = Nothing
stackPeek (Stack gs) = Just $ head gs
stackPop (Stack []) = Nothing
stackPop (Stack gs) = Just (Stack $ tail gs, head gs)

dropGarbage :: String -> String
dropGarbage "" = ""
dropGarbage stream = case head stream of
    '<' -> dropGarbage $ tail stream
    '!' -> dropGarbage $ drop 2 stream
    '>' -> tail stream
    _   -> dropGarbage $ tail stream

newGroup stack = case stackPeek stack of
    Just (Group s) -> stackPush stack $ Group $ s + 1
    Nothing -> stackPush stack $ Group 1

findGroupsRec :: Memory -> String -> Memory
findGroupsRec mem "" = mem
findGroupsRec (Memory stack done) stream = case head stream of
    '{' -> findGroupsRec (Memory (newGroup stack) done) $ tail stream
    '}' -> case stackPop stack of
        Just (s, g) -> findGroupsRec (Memory s (g : done)) $ tail stream
        Nothing -> error "empty stack"
    ',' -> findGroupsRec (Memory stack done) (tail stream)
    _   -> findGroupsRec (Memory stack done) (dropGarbage stream)

findGroups :: String -> [Group]
findGroups stream = do
    let mem = findGroupsRec (Memory stackNew []) stream
    groups mem
