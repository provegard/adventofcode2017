module Lib.Tests where
import Test.Hspec
import Lib

main = hspec $ do
    describe "generate" $ do
        it "generates a sequence" $ do
            (take 5 (generate 16807 65)) `shouldBe` [1092455, 1181022009, 245556042, 1744312007, 1352636452]
    describe "generateFiltered" $ do
        it "generates a sequence" $ do
            (take 5 (generateFiltered 4 16807 65)) `shouldBe` [1352636452, 1992081072, 530830436, 1980017072, 740335192]
    describe "valuesMatch" $ do
        it "finds a match" $ do
            (valuesMatch (245556042::Int) (1431495498::Int)) `shouldBe` True
        it "finds a non-match" $ do
            (valuesMatch (1092455::Int) (430625591::Int)) `shouldBe` False
