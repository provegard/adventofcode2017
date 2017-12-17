module Lib.Tests where
import Test.Hspec
import Lib
import qualified Data.Sequence as Seq
import Data.Maybe

main = hspec $ do
    describe "spinLock" $ do
        it "starts with 0" $ do
            (spinLock 0 3) `shouldBe` (0, Seq.fromList [0])
        it "jumps once" $ do
            (spinLock 1 3) `shouldBe` (1, Seq.fromList [0, 1])
        it "jumps twice" $ do
            (spinLock 2 3) `shouldBe` (1, Seq.fromList [0, 2, 1])
        it "jumps thrice" $ do
            (spinLock 3 3) `shouldBe` (2, Seq.fromList [0, 2, 3, 1])
        it "jumps a lot" $ do
            let (cp, s) = spinLock 2017 3
            let nextVal = Seq.index s (cp + 1)
            -- let nextVal = fromMaybe (-1 :: Int) nextValM
            nextVal `shouldBe` (638 :: Int)
