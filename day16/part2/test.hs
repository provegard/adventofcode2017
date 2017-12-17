module Lib.Tests where
import Test.Hspec
import Lib

dance5 :: [DanceMove] -> String
dance5 moves = do
    let dancers = createDancers 5
    let newDancers = foldl applyDanceMove dancers moves
    dancersToString newDancers

main = hspec $ do
    describe "parseInput" $ do
        it "parses sX" $ do
            (parseInput "s5") `shouldBe` [(Spin 5)]
        it "parses xA/B" $ do
            (parseInput "x5/6") `shouldBe` [(Exchange 5 6)]
        it "parses pA/B" $ do
            (parseInput "pp/e") `shouldBe` [(Partner 'p' 'e')]
        it "splits on comma" $ do
            (parseInput "s5,pe/b") `shouldBe` [(Spin 5), (Partner 'e' 'b')]
    describe "createDancers" $ do
        it "creates them" $ do
            let str = dancersToString $ createDancers 5
            str `shouldBe` "abcde"
    describe "applyDanceMove" $ do
        it "applies Spin" $ do
            let moves = [Spin 1]
            let retVal = dance5 moves
            retVal `shouldBe` "eabcd"
        it "applies Exchange" $ do
            let moves = [Exchange 3 4]
            let retVal = dance5 moves
            retVal `shouldBe` "abced"
        it "applies Partner" $ do
            let moves = [Partner 'e' 'b']
            let retVal = dance5 moves
            retVal `shouldBe` "aecdb"
        it "applies Exchange after Spin" $ do
            let moves = [Spin 1, Exchange 0 1]
            let retVal = dance5 moves
            retVal `shouldBe` "aebcd"
        it "applies Partner after Spin" $ do
            let moves = [Spin 1, Partner 'b' 'd']
            let retVal = dance5 moves
            retVal `shouldBe` "eadcb"
        it "applies Partner after Exchange" $ do
            let moves = [Exchange 3 4, Partner 'd' 'b']
            let retVal = dance5 moves
            retVal `shouldBe` "adceb"
        it "applies two Spins" $ do
            let moves = [Spin 1, Spin 1]
            let retVal = dance5 moves
            retVal `shouldBe` "deabc"
        it "applies two Partners" $ do
            let moves = [Partner 'a' 'b', Partner 'b' 'c']
            let retVal = dance5 moves
            retVal `shouldBe` "cabde"
    describe "finalOrderMulti" $ do
        it "works for the example, 1 iteration only" $ do
            (finalOrderMulti 1 5 "s1,x3/4,pe/b") `shouldBe` "baedc"
        it "works for the example, 2 iterations" $ do
            (finalOrderMulti 2 5 "s1,x3/4,pe/b") `shouldBe` "ceadb"
    