module Registers.Tests where
import Test.HUnit
import Registers
import qualified Data.Map as Map

ex1_ins = parse_instruction "b inc 5 if a > 1"
ex2_ins = parse_instruction "c inc -20 if c == 10"
ex3_ins = parse_instruction "a inc 1 if b < 5"

tests = test [
  "parse_instruction" ~: "ex 1" ~:
    (Instruction "b" "inc" 5 (Condition "a" ">" 1)) ~=? ex1_ins,
  "parse_instruction" ~: "ex 2" ~:
    (Instruction "c" "inc" (-20) (Condition "c" "==" 10)) ~=? ex2_ins,
  "apply_instruction" ~: "ex 1 to empty regs" ~:
    (Registers Map.empty 0) ~=? (apply_instruction ex1_ins $ Registers Map.empty 0),
  "apply_instruction" ~: "ex 2, failing cond" ~:
    (Registers (Map.fromList [("c", 9)]) 9) ~=? (apply_instruction ex2_ins $ Registers (Map.fromList [("c", 9)]) 9),
  "apply_instruction" ~: "ex 2, failing cond" ~:
    (Registers (Map.fromList [("c", (-10))]) 10) ~=? (apply_instruction ex2_ins $ Registers (Map.fromList [("c", 10)]) 10),
  "apply_instruction" ~: "ex 3" ~:
    (Registers (Map.fromList [("a", 1)]) 1) ~=? (apply_instruction ex3_ins $ Registers Map.empty 0)
  ]

main = runTestTT tests

