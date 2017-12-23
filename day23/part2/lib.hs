{-# LANGUAGE BangPatterns #-}
module Lib where
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map
import Data.Char
import Data.Maybe
import Debug.Trace

type Reg = Char
type RegOrValue = String
data Instruction = Set Reg RegOrValue
                 | Sub Reg RegOrValue
                 | Mul Reg RegOrValue
                 | Nop
                 | Jnz RegOrValue RegOrValue deriving (Eq, Show)
                 
toInt :: String -> Int
toInt x = read x :: Int

type Registers = Map.Map Char Int
data State = State Int Registers Int

second :: [a] -> a
second x = head $ tail x

parseInstruction' :: [String] -> Instruction
parseInstruction' [] = error "empty instruction"
parseInstruction' (x:rest)
    | x == "sub" = Sub (head (head rest)) (second rest)
    | x == "mul" = Mul (head (head rest)) (second rest)
    | x == "set" = Set (head (head rest)) (second rest)
    | x == "jnz" = Jnz (head rest) (second rest)
    | x == "nop" = Set 'a' "a"
    | otherwise  = error ("unknown instruction: " ++ x)
parseInstruction :: String -> Instruction
parseInstruction = parseInstruction' . words

emptyState :: State
emptyState = State 0 (Map.fromList [('a', 1)]) 0

regValue :: Registers -> Reg -> Int
regValue regs reg = fromMaybe 0 (Map.lookup reg regs)

getValue :: Registers -> RegOrValue -> Int
getValue regs x = if head x < 'a' then toInt x else fromMaybe 0 (Map.lookup (head x) regs)

updateRegValue :: (Int -> Int) -> Maybe Int -> Maybe Int 
updateRegValue f ma = Just (f (fromMaybe 0 ma))

executeInstruction :: State -> Instruction -> State
executeInstruction (State ptr regs mulCount) ins = case ins of
    Set reg value -> State next (Map.insert reg (getValue regs value) regs) mulCount
    Sub reg value -> State next (Map.alter (updateRegValue (\v -> v - getValue regs value)) reg regs) mulCount
    Mul reg value -> State next (Map.alter (updateRegValue (getValue regs value *)) reg regs) (mulCount + 1)
    Jnz n offs  -> do
        let v = getValue regs n
        let offset = getValue regs offs
        let np = if v /= 0 then ptr + offset else next
        State np regs mulCount
    -- x -> error ("unhandled instruction: " ++ show x)
    where next = 1 + ptr

run :: [String] -> Int
run instructionsAsStrings = do
    let s@(State _ regs _) = execute 0 emptyState
    --trace (dump s) $ 
    getValue regs "h"
    where
        instructions = Seq.fromList $ map parseInstruction instructionsAsStrings
        execute :: Int -> State -> State
        execute iter state@(State ptr regs _) | isDone iter ptr = state
        execute iter state@(State ptr _ _) = do
            let ins = Seq.index instructions ptr
            let nextState = executeInstruction state ins
            let nextState' = trace (dump nextState) nextState
            execute (iter + 1) nextState'
        isDone iter ptr = iter > 1000 || ptr < 0 || ptr >= length instructionsAsStrings
        dump (State ptr regs _) = show regs