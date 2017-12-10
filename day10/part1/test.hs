module Lib.Tests where
import Test.Hspec
import Lib
import qualified Data.Sequence as Seq
import Data.Foldable

main = hspec $ do
    describe "hash" $ do
        it "hashes" $ do
            (hash [0, 1, 2, 3, 4] [3, 4, 1, 5]) `shouldBe` [3, 4, 2, 1, 0]
    describe "takeC" $ do
        it "takes from a list" $ do
            (toList $ takeC (Seq.fromList [0, 1, 2]) 0 2) `shouldBe` [0, 1]
        it "wraps around" $ do
            (toList $ takeC (Seq.fromList [0, 1, 2]) 2 2) `shouldBe` [2, 0]
    describe "updateC"  $ do
        it "updates a list" $ do
            (toList $ updateC (Seq.fromList [9, 9, 9]) 0 (Seq.fromList [2, 3])) `shouldBe` [2, 3, 9]
        it "wraps around" $ do
            (toList $ updateC (Seq.fromList [9, 9, 9]) 2 (Seq.fromList [5, 4])) `shouldBe` [4, 9, 5]
    describe "verHash" $ do
        it "verifies" $ do
            (verHash [0, 1, 2, 3, 4] [3, 4, 1, 5]) `shouldBe` (12 :: Int)