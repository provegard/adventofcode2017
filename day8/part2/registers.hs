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

data Registers = Registers { reg_regs :: Map.Map String Int
                           , max_held :: Int
                           } deriving (Eq, Show)

to_int x = read x :: Int

max_register_value :: (Map.Map String Int) -> Int
max_register_value regs = maximum $ Map.elems regs

parse_instruction :: String -> Instruction
parse_instruction line = parse' $ words line
  where
    parse' (n:op1:d:_:cn:oper:v:_) = Instruction n op1 (to_int d) (Condition cn oper (to_int v))
    parse' x                      = error "malformed input"

get_register_value :: String -> Registers -> Int
get_register_value name (Registers regs _) = fromMaybe 0 $ Map.lookup name regs

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
    Registers reg_map new_max
  else
    regs
  where
    new_value = (make_fun operation) (get_register_value name regs) delta
    reg_map = Map.insert name new_value $ reg_regs regs
    new_max = max (max_register_value reg_map) (max_held regs)

apply_instructions_ :: [Instruction] -> Registers -> Registers
apply_instructions_ [] regs = regs
apply_instructions_ instructions regs = apply_instructions_ (tail instructions) (apply_instruction (head instructions) regs)

apply_instructions :: [Instruction] -> Registers
apply_instructions instructions = apply_instructions_ instructions (Registers Map.empty 0)

