module Lib.Tests where
import Test.Hspec
import Lib
import qualified Data.Map.Strict as Map

main = hspec $ do
    describe "parseLint" $ do
        it "parses" $ do
            (parseLine "1/2") `shouldBe` (Component 1 2)
    describe "build'" $ do
        it "builds a single port" $ do
            let ports = [Component 0 2]
            let actual = build' ports
            actual `shouldBe` [[Component 0 2]]
        it "builds from two matching ports" $ do
            let ports = [Component 0 2, Component 2 3]
            let actual = build' ports
            actual `shouldBe` [[Component 0 2], [Component 0 2, Component 2 3]]
        it "builds from two start components" $ do
            let ports = [Component 0 2, Component 0 4]
            let actual = build' ports
            actual `shouldBe` [[Component 0 2], [Component 0 4]]
    describe "findStrongest" $ do
        it "works for the example" $ do
            let strings = ["0/2", "2/2", "2/3", "3/4", "3/5", "0/1", "10/1", "9/10"]
            let (s, b) = findStrongest strings
            s `shouldBe` 31