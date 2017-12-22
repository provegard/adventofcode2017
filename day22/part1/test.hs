module Lib.Tests where
import Test.Hspec
import Lib
import qualified Data.Map.Strict as Map

main = hspec $ do
    describe "parseWorld" $ do
        it "parses" $ do
            let input = ["..#", "#..", "..."]
            let (World grid pos dir inf) = parseWorld input
            pos `shouldBe` (Position (1,1))
            dir `shouldBe` North
            inf `shouldBe` 0
            grid `shouldBe` (Grid $ Map.fromList [(Position (0,0), Node Clean),
                                                  (Position (0,1), Node Clean),
                                                  (Position (0,2), Node Infected),
                                                  (Position (1,0), Node Infected),
                                                  (Position (1,1), Node Clean),
                                                  (Position (1,2), Node Clean),
                                                  (Position (2,0), Node Clean),
                                                  (Position (2,1), Node Clean),
                                                  (Position (2,2), Node Clean)
                                                  ])
    describe "burst" $ do
        let input = ["..#", "#..", "..."]
        let world = parseWorld input
        it "bursts 1" $ do
            let (World _ _ _ inf) = burst world 1
            inf `shouldBe` 1
        it "bursts 2" $ do
            let (World _ _ _ inf) = burst world 2
            inf `shouldBe` 1
                