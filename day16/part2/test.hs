module Lib.Tests where
import Test.Hspec
import Lib
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.STRef

main = hspec $ do
    describe "parseInput" $ do
        it "parses sX" $ do
            (parseInput "s5") `shouldBe` [(Swap 5)]
        it "parses xA/B" $ do
            (parseInput "x5/6") `shouldBe` [(Exchange 5 6)]
        it "parses pA/B" $ do
            (parseInput "pp/e") `shouldBe` [(Partner 'p' 'e')]
        it "splits on comma" $ do
            (parseInput "s5,pe/b") `shouldBe` [(Swap 5), (Partner 'e' 'b')]
    describe "createDancers" $ do
        it "creates them" $ do
            (createDancers 5) `shouldBe` (listArray (0,4) "abcde")
    describe "applyDanceMove" $ do
        it "applies Swap" $ do
            let retVal = runSTUArray $ do
                dan <- thaw $ createDancers 5
                applyDanceMove dan (Swap 1)
                return dan
            retVal `shouldBe` (listArray (0,4) "eabcd")
        it "applies Exchange" $ do
            let retVal = runSTUArray $ do
                dan <- thaw $ createDancers 5
                applyDanceMove dan (Exchange 3 4)
                return dan
            retVal `shouldBe` (listArray (0,4) "abced")
        it "applies Partner" $ do
            let retVal = runSTUArray $ do
                dan <- thaw $ createDancers 5
                applyDanceMove dan (Partner 'e' 'b')
                return dan
            retVal `shouldBe` (listArray (0,4) "aecdb")
    describe "finalOrderMulti" $ do
        it "works for the example" $ do
            (finalOrderMulti 2 5 "s1,x3/4,pe/b") `shouldBe` "ceadb"