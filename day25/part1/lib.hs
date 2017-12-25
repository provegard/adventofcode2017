{-# LANGUAGE BangPatterns #-}
module Lib where
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.List.Split
import Data.List
import qualified Data.Set as Set
import Data.Char (isSpace)

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

newtype Tape = Tape (Map.Map Int Int) deriving (Eq, Show)

data Rule = Rule { valueToWrite :: Int, moveOffset :: Int, nextState :: String } deriving (Eq, Show)

data State = State { stateName :: String, rule0 :: Rule, rule1 :: Rule } deriving (Eq, Show)

newtype States = States (Map.Map String State) deriving (Eq, Show)

data Machine = Machine { tape :: Tape, cursor :: Int, currentState :: String, stepsLeft :: Int, states :: States } deriving (Eq, Show)

newTape = Tape Map.empty

newStates = States Map.empty

toInt x = read x :: Int

splitWords :: String -> [String]
splitWords s = splitOneOf " .:" (trim s)

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
        

parse m x rest = (m, rest)

parseInput :: [String] -> Machine
parseInput inputLines = do
    let machine = Machine newTape 0 "" 0 newStates
    --foldl parseLine machine inputLines
    parseLines machine inputLines
    where
        parseLines m [] = m
        parseLines m [x] = do
            let (m', _) = parse m (splitWords x) []
            m'
        parseLines m (l:rest) = do
            let (m', newRest) = parse m (splitWords l) rest
            parseLines m' newRest

valueAtCursor :: Machine -> Int
valueAtCursor m = do
    let (Tape tm) = tape m
    fromMaybe 0 $ Map.lookup (cursor m) tm

getCurrentState :: Machine -> State
getCurrentState m = do
    let (States sm) = states m
    let s = currentState m
    fromMaybe (error ("Unknown state: " ++ s)) $ Map.lookup s sm

updateTape :: Machine -> Int -> Tape
updateTape m value = do
    let (Tape tm) = tape m
    let newMap = if value == 0 then Map.delete (cursor m) tm else Map.insert (cursor m) value tm
    Tape newMap 

execute :: Machine -> Machine
execute m | stepsLeft m == 0 = m
execute m = do
    let curVal = valueAtCursor m
    let (State _ r0 r1) = getCurrentState m
    let rule = if curVal == 0 then r0 else r1
    let newTape = updateTape m (valueToWrite rule)
    let newCursor = cursor m + moveOffset rule
    let newState = nextState rule
    execute $ Machine newTape newCursor newState (stepsLeft m - 1) (states m)

checksum :: Machine -> Int
checksum m = do
    let (Tape tm) = tape m
    Map.foldl (+) 0 tm

executeInput :: [String] -> Machine
executeInput strings = do
    let machine = parseInput strings
    execute machine