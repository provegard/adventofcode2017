module Lib.Tests where
import Test.Hspec
import Lib
import qualified Data.Sequence as Seq

main = hspec $ do
    describe "parseInput" $ do
        it "parses sX" $ do
            (parseInput "s5") `shouldBe` [(Swap 5)]
        it "parses xA/B" $ do
            (parseInput "x5/6") `shouldBe` [(Exchange 5 6)]
        it "parses pA/B" $ do
            (parseInput "pp/e") `shouldBe` [(Partner "p" "e")]
        it "splits on comma" $ do
            (parseInput "s5,pe/b") `shouldBe` [(Swap 5), (Partner "e" "b")]
    describe "applyDanceMove" $ do
        it "applies Swap" $ do
            (applyDanceMove (createDancers 5) (Swap 1)) `shouldBe` (Seq.fromList ["e","a","b","c","d"])
        it "applies Exchange" $ do
            (applyDanceMove (createDancers 5) (Exchange 3 4)) `shouldBe` (Seq.fromList ["a","b","c","e","d"])
        it "applies Partner" $ do
            (applyDanceMove (createDancers 5) (Partner "e" "b")) `shouldBe` (Seq.fromList ["a","e","c","d","b"])
    describe "finalOrderMulti" $ do
        it "works for the example" $ do
            (finalOrderMulti 2 5 "s1,x3/4,pe/b") `shouldBe` "ceadb"