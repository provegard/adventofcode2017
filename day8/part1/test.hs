module Registers.Tests where
import Test.HUnit
import Registers
import qualified Data.Map as Map

ex1_ins = parse_instruction "b inc 5 if a > 1"
ex2_ins = parse_instruction "c inc -20 if c == 10"

tests = test [
  "parse_instruction" ~: "ex 1" ~:
    (Instruction "b" "inc" 5 (Condition "a" ">" 1)) ~=? ex1_ins,
  "parse_instruction" ~: "ex 2" ~:
    (Instruction "c" "inc" (-20) (Condition "c" "==" 10)) ~=? ex2_ins,
  "apply_instruction" ~: "ex 1 to empty regs" ~:
    Map.empty ~=? (apply_instruction ex1_ins Map.empty),
  "apply_instruction" ~: "ex 2, failing cond" ~:
    (Map.fromList [("c", 9)]) ~=? (apply_instruction ex2_ins $ Map.fromList [("c", 9)]),
  "apply_instruction" ~: "ex 2, failing cond" ~:
    (Map.fromList [("c", (-10))]) ~=? (apply_instruction ex2_ins $ Map.fromList [("c", 10)])
  ]

main = runTestTT tests

