module Lib.Tests where
import Test.Hspec
import Lib
import qualified Data.Sequence as Seq

main = hspec $ do
    describe "startingPosition" $ do
        it "finds the position" $ do
            let row = Seq.fromList "   |   "
            let rows = Seq.fromList [row]
            let pos = startingPosition rows
            pos `shouldBe` (Position 0 3 South)
    describe "walk" $ do
        it "walks south" $ do
            let row1 = Seq.fromList "   |   "
            let row2 = Seq.fromList "   |   "
            let rows = Seq.fromList [row1, row2]
            let pos = startingPosition rows
            let (p, c) = walk rows pos ""
            p `shouldBe` (Position 2 3 South)
        it "walks south until blank" $ do
            let row1 = Seq.fromList "   |   "
            let row2 = Seq.fromList "   |   "
            let row3 = Seq.fromList "       "
            let rows = Seq.fromList [row1, row2, row3]
            let pos = startingPosition rows
            let (p, c) = walk rows pos ""
            p `shouldBe` (Position 2 3 South)
        it "collects letters" $ do
            let row1 = Seq.fromList "   |   "
            let row2 = Seq.fromList "   A   "
            let row3 = Seq.fromList "   |   "
            let row4 = Seq.fromList "   B   "
            let rows = Seq.fromList [row1, row2, row3, row4]
            let pos = startingPosition rows
            let (p, c) = walk rows pos ""
            c `shouldBe` "AB"
        it "turns east" $ do
            let row1 = Seq.fromList "   |   "
            let row2 = Seq.fromList "   |   "
            let row3 = Seq.fromList "   |   "
            let row4 = Seq.fromList "   +-  "
            let rows = Seq.fromList [row1, row2, row3, row4]
            let pos = startingPosition rows
            let (p, c) = walk rows pos ""
            p `shouldBe` (Position 3 5 East)
        it "turns west" $ do
            let row1 = Seq.fromList "   |   "
            let row2 = Seq.fromList "   |   "
            let row3 = Seq.fromList "   |   "
            let row4 = Seq.fromList "  -+   "
            let rows = Seq.fromList [row1, row2, row3, row4]
            let pos = startingPosition rows
            let (p, c) = walk rows pos ""
            p `shouldBe` (Position 3 1 West)
    describe "isFinished" $ do
        it "returns False for a valid pos" $ do
            let row1 = Seq.fromList "   |   "
            let row2 = Seq.fromList "   +-  "
            let rows = Seq.fromList [row1, row2]
            let pos = Position 1 4 East
            (isFinished rows pos) `shouldBe` False
        it "returns True for a blank pos" $ do
            let row1 = Seq.fromList "   |   "
            let row2 = Seq.fromList "   +-  "
            let rows = Seq.fromList [row1, row2]
            let pos = Position 1 2 West
            (isFinished rows pos) `shouldBe` True
