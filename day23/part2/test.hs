module Lib.Tests where
import Test.Hspec
import Lib
import qualified Data.Map.Strict as Map

main = hspec $ do
    describe "parseInstruction" $ do
        it "parses sub" $ (parseInstruction "sub a 440") `shouldBe` (Sub 'a' "440")
        it "parses mul" $ (parseInstruction "mul b 2") `shouldBe` (Mul 'b' "2")
        it "parses set" $ (parseInstruction "set a 2") `shouldBe` (Set 'a' "2")
        it "parses jnz" $ (parseInstruction "jnz 1 2" `shouldBe` (Jnz "1" "2"))
    describe "executeInstruction" $ do
        it "executes set" $ do
            let (State _ regs) = executeInstruction emptyState (Set 'a' "1")
            (Map.!) regs 'a' `shouldBe` 1
        it "executes sub" $ do
            let s1 = executeInstruction emptyState (Set 'a' "1")
            let (State _ regs) = executeInstruction s1 (Sub 'a' "2")
            (Map.!) regs 'a' `shouldBe` (-1)
        it "executes mul" $ do
            let s1 = executeInstruction emptyState (Set 'a' "2")
            let (State _ regs) = executeInstruction s1 (Mul 'a' "2")
            (Map.!) regs 'a' `shouldBe` 4
        it "executes sub on empty" $ do
            let (State _ regs) = executeInstruction emptyState (Sub 'a' "2")
            (Map.!) regs 'a' `shouldBe` (-2)
        it "executes jnz (reg)" $ do
            let s1 = executeInstruction emptyState (Set 'a' "1")
            let (State p _) = executeInstruction s1 (Jnz "a" "3")
            p `shouldBe` 4
        it "ignores jnz (reg is zero)" $ do
            let (State p _) = executeInstruction emptyState (Jnz "a" "3")
            p `shouldBe` 1 -- next
        it "executes jnz (value)" $ do
            let (State p _) = executeInstruction emptyState (Jnz "1" "3")
            p `shouldBe` 3
        it "ignores jnz (value is zero)" $ do
            let (State p _) = executeInstruction emptyState (Jnz "0" "3")
            p `shouldBe` 1 -- next
        