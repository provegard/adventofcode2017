module Lib.Tests where
import Test.Hspec
import Lib
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

emptyState :: State
emptyState = State 0 Map.empty [] [] False

main = hspec $ do
    describe "parseInstruction" $ do
        it "parses snd" $ (parseInstruction "snd a") `shouldBe` (Snd "a")
        it "parses add" $ (parseInstruction "add a 440") `shouldBe` (Add 'a' "440")
        it "parses mul" $ (parseInstruction "mul b 2") `shouldBe` (Mul 'b' "2")
        it "parses mod" $ (parseInstruction "mod a 440") `shouldBe` (Mod 'a' "440")
        it "parses set" $ (parseInstruction "set a 2") `shouldBe` (Set 'a' "2")
        it "parses rcv" $ (parseInstruction "rcv i" `shouldBe` (Rcv 'i'))
        it "parses jgz" $ (parseInstruction "jgz 1 2" `shouldBe` (Jgz "1" "2"))
    describe "executeInstruction" $ do
        it "executes set" $ do
            let (State _ regs _ _ _) = executeInstruction emptyState (Set 'a' "1")
            (Map.!) regs 'a' `shouldBe` 1
        it "executes add" $ do
            let s1 = executeInstruction emptyState (Set 'a' "1")
            let (State _ regs _ _ _) = executeInstruction s1 (Add 'a' "2")
            (Map.!) regs 'a' `shouldBe` 3
        it "executes mul" $ do
            let s1 = executeInstruction emptyState (Set 'a' "2")
            let (State _ regs _ _ _) = executeInstruction s1 (Mul 'a' "2")
            (Map.!) regs 'a' `shouldBe` 4
        it "executes mod" $ do
            let s1 = executeInstruction emptyState (Set 'a' "5")
            let (State _ regs _ _ _) = executeInstruction s1 (Mod 'a' "2")
            (Map.!) regs 'a' `shouldBe` 1
        it "executes snd" $ do
            let s1 = executeInstruction emptyState (Set 'a' "5")
            let (State _ _ _ outgoing _) = executeInstruction s1 (Snd "a")
            outgoing `shouldBe` [5]
        it "executes add on empty" $ do
            let (State _ regs _ _ _) = executeInstruction emptyState (Add 'a' "2")
            (Map.!) regs 'a' `shouldBe` 2
        it "executes snd on empty" $ do
            let (State _ _ _ outgoing _) = executeInstruction emptyState (Snd "a")
            outgoing `shouldBe` [0]
        it "executes snd with number" $ do
            let (State _ _ _ outgoing _) = executeInstruction emptyState (Snd "5")
            outgoing `shouldBe` [5]
        it "executes snd twice" $ do
            let s1 = executeInstruction emptyState (Snd "5")
            let s2 = executeInstruction s1 (Snd "6")
            outgoing s2 `shouldBe` [5, 6]
        it "executes rcv when incoming is non-empty" $ do
            let s = State 0 Map.empty [5] [] False
            let (State ptr regs _ _ waiting) = executeInstruction s (Rcv 'a')
            (Map.!) regs 'a' `shouldBe` 5
            waiting `shouldBe` False
            ptr `shouldBe` 1
        it "executes rcv when incoming is empty" $ do
            let (State ptr _ [] _ waiting) = executeInstruction emptyState (Rcv 'a')
            waiting `shouldBe` True
            ptr `shouldBe` 0
        it "executes jgz (reg)" $ do
            let s1 = executeInstruction emptyState (Set 'a' "1")
            let (State p _ _ _ _) = executeInstruction s1 (Jgz "a" "3")
            p `shouldBe` 4
        it "ignores jgz (reg is zero)" $ do
            let (State p _ _ _ _) = executeInstruction emptyState (Jgz "a" "3")
            p `shouldBe` 1 -- next
        it "ignores jgz (reg is <0)" $ do
            let s1 = executeInstruction emptyState (Set 'a' "-1")
            let (State p _ _ _ _) = executeInstruction s1 (Jgz "a" "3")
            p `shouldBe` 2 -- next
        it "executes jgz (value)" $ do
            let (State p _ _ _ _) = executeInstruction emptyState (Jgz "1" "3")
            p `shouldBe` 3
        it "ignores jgz (value is zero)" $ do
            let (State p _ _ _ _) = executeInstruction emptyState (Jgz "0" "3")
            p `shouldBe` 1 -- next
    describe "sentByP1" $ do
        it "works for the example" $ do
            let ins = ["snd 1", "snd 2", "snd p", "rcv a", "rcv b", "rcv c", "rcv d"]
            let result = sentByP1 ins
            result `shouldBe` 3
    describe "runProcess" $ do
        it "records count of messages sent" $ do
            let p = newProcess 0
            let ins = Seq.fromList [Snd "1", Snd "2", Snd "3", Rcv 'a']
            let p2 = runProcess p ins
            (sent p2) `shouldBe` 3
        it "leaves messages sent in outgoing" $ do
            let p = newProcess 0
            let ins = Seq.fromList [Snd "1", Snd "2", Snd "3", Rcv 'a']
            let p2 = runProcess p ins
            let msgs = (outgoing . processState) p2
            msgs `shouldBe` [1,2,3]
        it "stops on Rcv" $ do
            let p = newProcess 0
            let ins = Seq.fromList [Set 'a' "1", Rcv 'a']
            let p2 = runProcess p ins
            waiting (processState p2) `shouldBe` True
            cp (processState p2) `shouldBe` 1
        it "sends one" $ do
            let p = newProcess 0
            let ins = Seq.fromList [Snd "1", Rcv 'a']
            let p2 = runProcess p ins
            let msgs = (outgoing . processState) p2
            msgs `shouldBe` [1]
                