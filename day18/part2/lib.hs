{-# LANGUAGE BangPatterns #-}
module Lib where
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map
import Data.Char
import Data.Maybe

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

data System = System Process Process
type ProcessId = Int
data Process = Process { processId :: ProcessId, processState :: State, sent :: Int }

parseInstruction' :: [String] -> Instruction
parseInstruction' (x:y:rest)
    | x == "snd" = Snd y
    | x == "add" = Add (head y) (head rest)
    | x == "mul" = Mul (head y) (head rest)
    | x == "mod" = Mod (head y) (head rest)
    | x == "set" = Set (head y) (head rest)
    | x == "rcv" = Rcv (head y)
    | x == "jgz" = Jgz y (head rest)
    | otherwise  = error ("unknown instruction: " ++ x)
parseInstruction' x = error ("malformed instruction: " ++ concat x)
parseInstruction :: String -> Instruction
parseInstruction = parseInstruction' . words

newStateForProcess :: ProcessId -> State
newStateForProcess pid = State 0 (Map.fromList [('p', pid)]) [] [] False

getValue :: Registers -> RegOrValue -> Int
getValue regs x = if head x < 'a' then toInt x else fromMaybe 0 (Map.lookup (head x) regs)

executeInstruction :: State -> Instruction -> State
executeInstruction (State ptr regs incoming outgoing waiting) ins = case ins of
    Set reg value -> State next (modRegs reg const value) incoming outgoing False
    Add reg value -> State next (modRegs reg (+) value) incoming outgoing False
    Mul reg value -> State next (modRegs reg (*) value) incoming outgoing False
    Mod reg value -> State next (modRegs reg rmod value) incoming outgoing False
    Snd value     -> State next regs incoming (outgoing ++ [regValue value]) False
    Rcv reg       ->
        if null incoming then
            State ptr regs incoming outgoing True -- wait, ptr is same!
        else do
            let receivedValue = head incoming
            let newRegs = Map.insert reg receivedValue regs
            State next newRegs (tail incoming) outgoing False
    Jgz n offs    -> do
        let v = regValue n
        let offset = regValue offs
        let np = if v <= 0 then next else ptr + offset
        State np regs incoming outgoing False
    where 
        next = 1 + ptr
        regValue = getValue regs
        modRegs reg f value = Map.alter ((updateRegValue . f . regValue) value) reg regs
        rmod x y = y `mod` x -- don't know how to pass the standard mod fun to modRegs
        updateRegValue f ma = Just (f (fromMaybe 0 ma))
        
newProcess :: ProcessId -> Process
newProcess pid = let s = newStateForProcess pid
                 in Process pid s 0

canRun :: Process -> Bool
canRun (Process _ state _) = not (waiting state && null (incoming state))

newSystem :: System
newSystem = System (newProcess 0) (newProcess 1)

runProcess :: Process -> Seq.Seq Instruction -> Process
runProcess (Process pid !initialState sent) !instructions = do
    let newState = execute initialState
    let newSent = sent + length (outgoing newState)
    Process pid newState newSent
    where
        execute state = do
            let ins = Seq.index instructions (cp state)
            let newState = executeInstruction state ins
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
runUntilDeadlock system@(System p0 p1) instructions
    | canRun p0 = do
        let newP0 = runProcess p0 instructions
        let (newP0', newP2) = transfer newP0 p1
        runUntilDeadlock (System newP0' newP2) instructions
    | canRun p1 = do
        let newP1 = runProcess p1 instructions
        let (newP1', newP0) = transfer newP1 p0
        runUntilDeadlock (System newP0 newP1') instructions
    | otherwise = system -- deadlock

sentByP1 :: [String] -> Int
sentByP1 strInstructions = do
    let instructions = Seq.fromList $ map parseInstruction strInstructions
    let (System _ p1) = runUntilDeadlock newSystem instructions
    sent p1