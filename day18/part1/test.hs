module Lib.Tests where
import Test.Hspec
import Lib
import qualified Data.Map.Strict as Map

main = hspec $ do
    describe "parseInstruction" $ do
        it "parses snd" $ (parseInstruction "snd a") `shouldBe` (Snd 'a')
        it "parses add" $ (parseInstruction "add a 440") `shouldBe` (Add 'a' "440")
        it "parses mul" $ (parseInstruction "mul b 2") `shouldBe` (Mul 'b' "2")
        it "parses mod" $ (parseInstruction "mod a 440") `shouldBe` (Mod 'a' "440")
        it "parses set" $ (parseInstruction "set a 2") `shouldBe` (Set 'a' "2")
        it "parses rcv" $ (parseInstruction "rcv i" `shouldBe` (Rcv 'i'))
        it "parses jgz" $ (parseInstruction "jgz 1 2" `shouldBe` (Jgz "1" 2))
    describe "executeInstruction" $ do
        it "executes set" $ do
            let (State _ regs _ _) = executeInstruction emptyState (Set 'a' "1")
            (Map.!) regs 'a' `shouldBe` 1
        it "executes add" $ do
            let s1 = executeInstruction emptyState (Set 'a' "1")
            let (State _ regs _ _) = executeInstruction s1 (Add 'a' "2")
            (Map.!) regs 'a' `shouldBe` 3
        it "executes mul" $ do
            let s1 = executeInstruction emptyState (Set 'a' "2")
            let (State _ regs _ _) = executeInstruction s1 (Mul 'a' "2")
            (Map.!) regs 'a' `shouldBe` 4
        it "executes mod" $ do
            let s1 = executeInstruction emptyState (Set 'a' "5")
            let (State _ regs _ _) = executeInstruction s1 (Mod 'a' "2")
            (Map.!) regs 'a' `shouldBe` 1
        it "executes snd" $ do
            let s1 = executeInstruction emptyState (Set 'a' "5")
            let (State _ _ lastPlayed _) = executeInstruction s1 (Snd 'a')
            lastPlayed `shouldBe` Just 5
        it "executes add on empty" $ do
            let (State _ regs _ _) = executeInstruction emptyState (Add 'a' "2")
            (Map.!) regs 'a' `shouldBe` 2
        it "executes snd on empty" $ do
            let (State _ _ lastPlayed _) = executeInstruction emptyState (Snd 'a')
            lastPlayed `shouldBe` Just 0
        it "executes rcv with non-zero" $ do
            let s1 = executeInstruction emptyState (Add 'a' "2")
            let s2 = executeInstruction s1 (Snd 'a')
            let (State _ _ _ lastReceived) = executeInstruction s2 (Rcv 'a')
            lastReceived `shouldBe` Just 2
        it "executes rcv with zero" $ do
            let s1 = executeInstruction emptyState (Add 'a' "2")
            let s2 = executeInstruction s1 (Snd 'a')
            let (State _ _ _ lastReceived) = executeInstruction s2 (Rcv 'b')
            lastReceived `shouldBe` Nothing
        it "executes jgz (reg)" $ do
            let s1 = executeInstruction emptyState (Set 'a' "1")
            let (State p _ _ _) = executeInstruction s1 (Jgz "a" 3)
            p `shouldBe` 4
        it "ignores jgz (reg is zero)" $ do
            let (State p _ _ _) = executeInstruction emptyState (Jgz "a" 3)
            p `shouldBe` 1 -- next
        it "executes jgz (value)" $ do
            let (State p _ _ _) = executeInstruction emptyState (Jgz "1" 3)
            p `shouldBe` 3
        it "ignores jgz (value is zero)" $ do
            let (State p _ _ _) = executeInstruction emptyState (Jgz "0" 3)
            p `shouldBe` 1 -- next
        