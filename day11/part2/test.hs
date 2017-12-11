module Lib.Tests where
import Test.Hspec
import Lib

main = hspec $ do
    describe "stepDistance" $ do
        it "ne,ne,ne" $ do
            (stepDistance ["ne", "ne", "ne"]) `shouldBe` (3 :: Int)
        it "ne,ne,sw,sw" $ do
            (stepDistance ["ne", "ne", "sw", "sw"]) `shouldBe` (0 :: Int)
        it "ne,ne,s,s" $ do
            (stepDistance ["ne", "ne", "s", "s"]) `shouldBe` (2 :: Int)
        it "se,sw,se,sw,sw" $ do
            (stepDistance ["se", "sw", "se", "sw", "sw"]) `shouldBe` (3 :: Int)
            