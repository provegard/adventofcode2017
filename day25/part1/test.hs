module Lib.Tests where
import Test.Hspec
import Lib
import qualified Data.Map.Strict as Map

main = hspec $ do
    describe "parseInput" $ do
        it "parses the Begin line" $ do
            let m = parseInput ["Begin in state A."]
            (currentState m) `shouldBe` "A"
        it "parses the steps-left line" $ do
            let m = parseInput ["Perform a diagnostic checksum after 6 steps."]
            (stepsLeft m) `shouldBe` 6
        it "ignores an empty line" $ do
            let m = parseInput [""]
            (cursor m) `shouldBe` 0
        it "parses a state description" $ do
            let strings = ["In state A:",
                           "  If the current value is 0:",
                           "    - Write the value 1.",
                           "    - Move one slot to the right.",
                           "    - Continue with state B.",
                           "  If the current value is 1:",
                           "    - Write the value 0.",
                           "    - Move one slot to the left.",
                           "    - Continue with state B."]
            let m = parseInput strings
            let r0 = Rule 1 1 "B"
            let r1 = Rule 0 (-1) "B"
            let expMap = Map.fromList [("A", (State "A" r0 r1))]
            states m `shouldBe` (States expMap)