module Lib.Tests where
import Test.Hspec
import Lib

main = hspec $ do
    describe "countRegions" $ do
        it "works for the test input" $ do
            (countRegions "flqrgnkx") `shouldBe` (1242 :: Int)