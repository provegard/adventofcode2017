module Lib.Tests where
import Test.Hspec
import Lib

main = hspec $ do
    describe "moveScanner" $ do
        it "beginning" $ do
            (moveScanner $ newLayer 3) `shouldBe` (Layer 3 1 1)
        it "forward" $ do
            (moveScanner $ Layer 3 1 1) `shouldBe` (Layer 3 2 1)
        it "end" $ do
            (moveScanner $ Layer 3 2 1) `shouldBe` (Layer 3 1 (-1))
        it "backward" $ do
            (moveScanner $ Layer 3 1 (-1)) `shouldBe` (Layer 3 0 (-1))
    describe "readLayers" $ do
        it "parses a layer" $ do
            (readLayers ["1: 3"]) `shouldBe` [(1, Layer 3 0 1)]    
    describe "getSeverity" $ do
        it "works for the example" $ do
            let lines = ["0: 3", "1: 2", "4: 4", "6: 4"]
            (getSeverity lines) `shouldBe` (24 :: Int)