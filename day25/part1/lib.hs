{-# LANGUAGE BangPatterns #-}
module Lib where
import Data.Maybe
import Data.List.Split
import Data.List
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Char (isSpace)

newtype Tape = Tape (Set.Set Int) deriving (Eq, Show)
data Rule = Rule { valueToWrite :: Int, moveOffset :: Int, nextState :: String } deriving (Eq, Show)
data State = State { stateName :: String, rule0 :: Rule, rule1 :: Rule } deriving (Eq, Show)
newtype States = States (Map.Map String State) deriving (Eq, Show)
data Machine = Machine { tape :: Tape, cursor :: Int, currentState :: String, stepsLeft :: Int, states :: States } deriving (Eq, Show)


toInt x = read x :: Int
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace
splitWords :: String -> [String]
splitWords s = splitOneOf " .:" (trim s)

newTape = Tape Set.empty
newStates = States Map.empty
newMachine = Machine newTape 0 "" 0 newStates

parse :: Machine -> [String] -> [String] -> (Machine, [String])
parse m ("Begin":_:_:name:_) rest    = (m { currentState = name }, rest)
parse m ("Perform":_:_:_:_:n:_) rest = (m { stepsLeft = toInt n }, rest)
parse m ("In":_:name:_) rest = do
    let (descLines, newRest) = splitAt 8 rest
    let parts = map (parseDescLine . splitWords) descLines
    let (r0Lines, r1Lines) = splitAt 4 parts
    let r0 = buildRule r0Lines
    let r1 = buildRule r1Lines
    let state = State name r0 r1
    let (States stateMap) = states m
    let newStates = States $ Map.insert name state stateMap
    (m { states = newStates }, newRest)
    where
        parseDir x = if x == "right" then 1 else (-1)
        buildRule (_:w:m:c:_) = Rule (toInt w) (parseDir m) c
        buildRule x = error ("Unknown rule parts: " ++ show x)
        parseDescLine (_:"Write":_:_:n:_) = n
        parseDescLine (_:"Move":_:_:_:_:dir:_) = dir
        parseDescLine (_:"Continue":_:_:nxt:_) = nxt
        parseDescLine ("If":_:_:_:_:v:_) = v
        parseDescLine x = error ("Unknown description line: " ++ show x)
-- parse empty (or unknown) line
parse m x rest = (m, rest)

parseInput :: [String] -> Machine
parseInput = parseLines newMachine
    where
        parseLines m []       = m
        parseLines m [x]      = fst $ parse m (splitWords x) []
        parseLines m (l:rest) = uncurry parseLines $ parse m (splitWords l) rest

valueAtCursor :: Machine -> Int
valueAtCursor m = let (Tape ts) = tape m
                  in  if Set.member (cursor m) ts then 1 else 0

getCurrentState :: Machine -> State
getCurrentState m =
    let (States sm) = states m
        s = currentState m
    in  fromMaybe (error ("Unknown state: " ++ s)) $ Map.lookup s sm

updateTape :: Machine -> Int -> Tape
updateTape m value =
    let (Tape ts) = tape m
        newSet = (if value == 1 then Set.insert else Set.delete) (cursor m) ts
    in  Tape newSet

execute :: Machine -> Machine
execute m | stepsLeft m == 0 = m
execute m =
    let curVal = valueAtCursor m
        (State _ r0 r1) = getCurrentState m
        rule = if curVal == 0 then r0 else r1
        newTape = updateTape m (valueToWrite rule)
        newCursor = cursor m + moveOffset rule
        newState = nextState rule
    in  execute $ Machine newTape newCursor newState (stepsLeft m - 1) (states m)

checksum :: Machine -> Int
checksum m = let (Tape ts) = tape m
             in  Set.size ts

executeInput :: [String] -> Machine
executeInput strings = execute $ parseInput strings