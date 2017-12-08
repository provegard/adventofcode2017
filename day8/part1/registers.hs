module Registers where
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.List

data Condition = Condition { cond_name :: String
                           , cond_operator :: String
                           , cond_value :: Int
                           } deriving (Eq, Show)

data Instruction = Instruction { reg_name :: String
                               , ins_operation :: String
                               , ins_delta :: Int
                               , ind_cond :: Condition
                               } deriving (Eq, Show) 


type Registers = Map.Map String Int

to_int x = read x :: Int

parse_instruction :: String -> Instruction
parse_instruction line = parse' $ words line
  where
    parse' (n:op1:d:_:cn:oper:v:_) = Instruction n op1 (to_int d) (Condition cn oper (to_int v))
    parse' x                      = error "malformed input"

get_register_value :: String -> Registers -> Int
get_register_value name regs = fromMaybe 0 $ Map.lookup name regs

test_condition :: Condition -> Registers -> Bool
test_condition (Condition name operator value) regs = case operator of
    "<"  -> rv < value
    "==" -> rv == value
    "!=" -> rv /= value
    ">"  -> rv > value
    ">=" -> rv >= value
    "<=" -> rv <= value
    x    -> error ("unknown operator: " ++ x)
  where rv = get_register_value name regs

make_fun :: String -> (Int -> Int -> Int)
make_fun oper = case oper of
    "inc" -> (+)
    "dec" -> (-)
    x     -> error ("unknown operation: " ++ x)

apply_instruction :: Instruction -> Registers -> Registers
apply_instruction (Instruction name operation delta condition) regs = do
  if test_condition condition regs then
    Map.insert name new_value regs
  else
    regs
  where
    new_value = (make_fun operation) (get_register_value name regs) delta

apply_instructions :: [Instruction] -> Registers -> Registers
apply_instructions [] regs = regs
apply_instructions instructions regs = apply_instructions (tail instructions) (apply_instruction (head instructions) regs)

max_register_value :: Registers -> Int
max_register_value regs = maximum $ Map.elems regs

