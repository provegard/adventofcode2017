{-# LANGUAGE BangPatterns #-}
module Lib where
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map
import Data.Char
import Data.Maybe
import Debug.Trace

type Reg = Char
type RegOrValue = String
data Instruction = Snd RegOrValue
                 | Set Reg RegOrValue
                 | Add Reg RegOrValue
                 | Mul Reg RegOrValue
                 | Mod Reg RegOrValue
                 | Rcv Reg
                 | Jgz RegOrValue RegOrValue deriving (Eq, Show)
                 
toInt :: String -> Int
toInt x = read x :: Int

type Queue = [Int]

type Registers = Map.Map Char Int
-- curptr regs incoming outgoing waiting
data State = State { cp :: Int, regs :: Registers, incoming :: Queue, outgoing :: Queue, waiting :: Bool }

newtype System = System [Process]
type ProcessId = Int
data Process = Process { processId :: ProcessId, processState :: State, sent :: Int }


second :: [a] -> a
second x = head $ tail x

parseInstruction' :: [String] -> Instruction
parseInstruction' [] = error "empty instruction"
parseInstruction' (x:rest)
    | x == "snd" = Snd (head rest)
    | x == "add" = Add (head (head rest)) (second rest)
    | x == "mul" = Mul (head (head rest)) (second rest)
    | x == "mod" = Mod (head (head rest)) (second rest)
    | x == "set" = Set (head (head rest)) (second rest)
    | x == "rcv" = Rcv (head (head rest))
    | x == "jgz" = Jgz (head rest) (second rest)
    | otherwise  = error ("unknown instruction: " ++ x)
parseInstruction :: String -> Instruction
parseInstruction = parseInstruction' . words

emptyState :: State
emptyState = State 0 Map.empty [] [] False

newStateForProcess :: ProcessId -> State
newStateForProcess pid = State 0 (Map.fromList [('p', pid)]) [] [] False

regValue :: Registers -> Reg -> Int
regValue regs reg = fromMaybe 0 (Map.lookup reg regs)

getValue :: Registers -> RegOrValue -> Int
getValue regs x = if head x < 'a' then toInt x else fromMaybe 0 (Map.lookup (head x) regs)

updateRegValue :: (Int -> Int) -> Maybe Int -> Maybe Int 
updateRegValue f ma = Just (f (fromMaybe 0 ma))

executeInstruction :: State -> Instruction -> State
executeInstruction (State ptr regs incoming outgoing waiting) ins = case ins of
    Set reg value -> State next (Map.insert reg (getValue regs value) regs) incoming outgoing False
    Add reg value -> State next (Map.alter (updateRegValue (getValue regs value +)) reg regs) incoming outgoing False
    Mul reg value -> State next (Map.alter (updateRegValue (getValue regs value *)) reg regs) incoming outgoing False
    Mod reg value -> State next (Map.alter (updateRegValue (`mod` getValue regs value)) reg regs) incoming outgoing False
    Snd value     -> do
        -- add to outgoing
        let valueToSend = getValue regs value
        State next regs incoming (outgoing ++ [valueToSend]) False
    Rcv reg       -> case length incoming of
            0 -> State ptr regs incoming outgoing True -- wait, ptr is same!
            n -> do
                let receivedValue = head incoming
                let newRegs = Map.insert reg receivedValue regs
                State next newRegs (tail incoming) outgoing False
    Jgz n offs    -> do
        let v = getValue regs n
        let offset = getValue regs offs
        let np = if v <= 0 then next else ptr + offset
        State np regs incoming outgoing False
    where next = 1 + ptr

newProcess :: ProcessId -> Process
newProcess pid = let s = newStateForProcess pid
                 in Process pid s 0

canRun :: Process -> Bool
canRun (Process _ state _) = not (waiting state && null (incoming state))

newSystem :: System
newSystem = System [newProcess 0, newProcess 1]

runProcess :: Process -> Seq.Seq Instruction -> Process
runProcess (Process pid !initialState sent) !instructions = do
    let newState = execute initialState
    let newSent = sent + length (outgoing newState)
    -- let newSent = trace ("sent for " ++ show pid ++ " is now " ++ show newSent') newSent'
    Process pid newState newSent
    where
        execute state = do
            let ins = Seq.index instructions (cp state)
            let newState = executeInstruction state ins
            -- let newState = trace ("outgoing is now: " ++ (show (outgoing newState')) ++ " after " ++ show ins) newState'
            if waiting newState then newState else execute newState

transfer :: Process -> Process -> (Process, Process)
transfer !src !dst = do
    let s1 = processState src
    if null (outgoing s1) then
        (src, dst)
    else do
        let s2 = processState dst
        let toTransfer = outgoing s1
        let newS1 = State (cp s1) (regs s1) (incoming s1) [] (waiting s1)
        let newS2 = State (cp s2) (regs s2) (incoming s2 ++ toTransfer) (outgoing s2) (waiting s2)
        let newP1 = Process (processId src) newS1 (sent src)
        let newP2 = Process (processId dst) newS2 (sent dst)
        (newP1, newP2)

runUntilDeadlock :: System -> Seq.Seq Instruction -> System
runUntilDeadlock system@(System [p1,p2]) instructions
    | canRun p1 = do
        let newP1 = runProcess p1 instructions
        let (newP1', newP2) = transfer newP1 p2
        runUntilDeadlock (System [newP1', newP2]) instructions
    | canRun p2 = do
        let newP2 = runProcess p2 instructions
        let (newP2', newP1) = transfer newP2 p1
        runUntilDeadlock (System [newP1, newP2']) instructions
    | otherwise = system -- deadlock

sentByP1 :: [String] -> Int
sentByP1 strInstructions = do
    let system = newSystem
    let instructions = Seq.fromList $ map parseInstruction strInstructions
    let (System [_,p1]) = runUntilDeadlock system instructions
    sent p1