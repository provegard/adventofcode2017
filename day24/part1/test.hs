module Lib.Tests where
import Test.Hspec
import Lib
import qualified Data.Map.Strict as Map

main = hspec $ do
    describe "parseLint" $ do
        it "parses" $ do
            (parseLine "1/2") `shouldBe` (Port 1 2)
    describe "build'" $ do
        it "builds a single port" $ do
            let ports = [Port 1 2]
            let actual = build' ports
            actual `shouldBe` [[Port 1 2]]
        it "builds from two matching ports" $ do
            let ports = [Port 1 2, Port 2 3]
            let actual = build' ports
            actual `shouldBe` [[Port 1 2], [Port 1 2, Port 2 3], [Port 2 3]]
        it "builds from mismatching ports" $ do
            let ports = [Port 1 2, Port 3 4]
            let actual = build' ports
            actual `shouldBe` [[Port 1 2], [Port 3 4]]