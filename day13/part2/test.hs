module Lib.Tests where
import Test.Hspec
import Lib

main = hspec $ do
    describe "readLayers" $ do
        it "parses a layer" $ do
            (readLayers ["1: 3"]) `shouldBe` [(1, Layer 3)]    
    describe "getMinDelay" $ do
        it "works for the example" $ do
            let lines = ["0: 3", "1: 2", "4: 4", "6: 4"]
            (getMinDelay lines) `shouldBe` (10 :: Int)
