{-# LANGUAGE BangPatterns #-}
module Lib where
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map
import Data.Char
import Data.Maybe

type Reg = Char
type RegOrValue = String
data Instruction = Snd Reg
                 | Set Reg RegOrValue
                 | Add Reg RegOrValue
                 | Mul Reg RegOrValue
                 | Mod Reg RegOrValue
                 | Rcv Reg
                 | Jgz RegOrValue Int deriving (Eq, Show)
                 
toInt :: String -> Int
toInt x = read x :: Int

type Registers = Map.Map Char Int
data State = State Int Registers (Maybe Int) (Maybe Int)

second :: [a] -> a
second x = head $ tail x

parseInstruction' :: [String] -> Instruction
parseInstruction' [] = error "empty instruction"
parseInstruction' (x:rest)
    | x == "snd" = Snd $ head (head rest)
    | x == "add" = Add (head (head rest)) (second rest)
    | x == "mul" = Mul (head (head rest)) (second rest)
    | x == "mod" = Mod (head (head rest)) (second rest)
    | x == "set" = Set (head (head rest)) (second rest)
    | x == "rcv" = Rcv (head (head rest))
    | x == "jgz" = Jgz (head rest) (toInt (second rest))
    | otherwise  = error ("unknown instruction: " ++ x)
parseInstruction :: String -> Instruction
parseInstruction = parseInstruction' . words

emptyState :: State
emptyState = State 0 Map.empty Nothing Nothing

regValue :: Registers -> Reg -> Int
regValue regs reg = fromMaybe 0 (Map.lookup reg regs)

getValue :: Registers -> RegOrValue -> Int
getValue regs x = if head x < 'a' then toInt x else fromMaybe 0 (Map.lookup (head x) regs)

updateRegValue :: (Int -> Int) -> Maybe Int -> Maybe Int 
updateRegValue f ma = Just (f (fromMaybe 0 ma))

executeInstruction :: State -> Instruction -> State
executeInstruction (State ptr regs lastPlayed lastRecovered) ins = case ins of
    Set reg value -> State next (Map.insert reg (getValue regs value) regs) lastPlayed lastRecovered
    Add reg value -> State next (Map.alter (updateRegValue (getValue regs value +)) reg regs) lastPlayed lastRecovered
    Mul reg value -> State next (Map.alter (updateRegValue (getValue regs value *)) reg regs) lastPlayed lastRecovered
    Mod reg value -> State next (Map.alter (updateRegValue (`mod` getValue regs value)) reg regs) lastPlayed lastRecovered
    Snd reg       -> State next regs (Just (regValue regs reg)) lastRecovered
    Rcv reg       -> do
        let lp = if 0 == regValue regs reg then Nothing else lastPlayed
        State next regs lastPlayed lp
    Jgz n offset  -> do
        let v = getValue regs n
        let np = if v == 0 then next else ptr + offset
        State np regs lastPlayed lastRecovered
    -- x -> error ("unhandled instruction: " ++ show x)
    where next = 1 + ptr

recoverFrequency :: [String] -> Int
recoverFrequency instructionsAsStrings = do
    let instructions = Seq.fromList $ map parseInstruction instructionsAsStrings
    let execute state@(State ptr _ _ _) = do
        let ins = Seq.index instructions ptr
        let ns@(State _ _ _ recovered) = executeInstruction state ins
        fromMaybe (execute ns) recovered
    execute emptyState
