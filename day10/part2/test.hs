module Lib.Tests where
import Test.Hspec
import Lib
import qualified Data.Sequence as Seq
import Data.Foldable

main = hspec $ do
    -- describe "hash" $ do
    --     it "hashes" $ do
    --         (hash [0, 1, 2, 3, 4] [3, 4, 1, 5]) `shouldBe` [3, 4, 2, 1, 0]
    describe "takeC" $ do
        it "takes from a list" $ do
            toList (takeC (Seq.fromList [0, 1, 2]) 0 2) `shouldBe` [0, 1]
        it "wraps around" $ do
            toList (takeC (Seq.fromList [0, 1, 2]) 2 2) `shouldBe` [2, 0]
    describe "updateC"  $ do
        it "updates a list" $ do
            toList (updateC (Seq.fromList [9, 9, 9]) 0 (Seq.fromList [2, 3])) `shouldBe` [2, 3, 9]
        it "wraps around" $ do
            toList (updateC (Seq.fromList [9, 9, 9]) 2 (Seq.fromList [5, 4])) `shouldBe` [4, 9, 5]
    -- describe "verHash" $ do
    --     it "verifies" $ do
    --         (verHash [0, 1, 2, 3, 4] [3, 4, 1, 5]) `shouldBe` (12 :: Int)
    describe "parseInput" $ do
        it "parses empty" $ do
            (parseInput "") `shouldBe` [17, 31, 73, 47, 23]
        it "uses the ASCII code of all chars" $ do
            (parseInput "1,2,3") `shouldBe` [49,44,50,44,51,17, 31, 73, 47, 23]
    -- describe "dense" $ do
    --     it "xors things together" $ do
    --         (dense (Seq.fromList [65, 27, 9, 1, 4, 3, 40, 50, 91, 7, 6, 0, 2, 5, 68, 22])) `shouldBe` (64 :: Int)
    describe "makeDense" $ do
        it "generates a dense Seq" $ do
            (makeDense (Seq.fromList [65, 27, 9, 1, 4, 3, 40, 50, 91, 7, 6, 0, 2, 5, 68, 22])) `shouldBe` (Seq.fromList [64])
    describe "realHash" $ do
        it "hashes empty string" $ do
            (realHash "") `shouldBe` "a2582a3a0e66e6e86e3812dcb672a272"
        it "hashes 'AoC 2017'" $ do
            (realHash "AoC 2017") `shouldBe` "33efeb34ea91902bb2f59c9920caa6cd"
        it "hashes '1,2,3'" $ do
            (realHash "1,2,3") `shouldBe` "3efbe78a8d82f29979031a4aa0b16a9d"
        it "hashes '1,2,4'" $ do
            (realHash "1,2,4") `shouldBe` "63960835bcdc130f0b66d7ff4f6a5a8e"