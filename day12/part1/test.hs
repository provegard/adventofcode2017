module Lib.Tests where
import Test.Hspec
import Lib

main = hspec $ do
    describe "x" $ do
        it "y" $ do
            (1 + 2) `shouldBe` (3 :: Int)
            