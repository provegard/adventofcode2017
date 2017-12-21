module Lib.Tests where
import Test.Hspec
import Lib
import qualified Data.Sequence as Seq

main = hspec $ do
    describe "parseGrid" $ do
        it "parses a line" $ do
            let line = "###/##./#.#"
            let grid = parseGrid line
            grid `shouldBe` (Grid ["###", "##.", "#.#"])
    describe "parseRule" $ do
        it "parses a line" $ do
            let line = "../.. => ##./###/..."
            let rule = parseRule line
            rule `shouldBe` (Rule (Grid ["..", ".."]) (Grid ["##.", "###", "..."]))
    describe "flipGrid" $ do
        it "flips" $ do
            let input = Grid ["#..", "..#", ".#."]
            let flipped = flipGrid input
            flipped `shouldBe` (Grid ["..#", "#..", ".#."])
    describe "rotateGrid" $ do
        it "rotates" $ do
            let input = Grid [".#.", "..#", "###"]
            let rotated = rotateGrid input
            rotated `shouldBe` (Grid ["#..", "#.#", "##."])
    describe "splitGrid" $ do
        it "splits" $ do
            let input = Grid ["#..#", "....", "....", "#..#"]
            let result = splitGrid input
            result `shouldBe` [Grid ["#.", ".."], Grid [".#", ".."], Grid ["..", "#."], Grid ["..", ".#"]]
    describe "pixelsOn" $ do
        it "counts" $ do
            let input = Grid ["#..#", "....", "....", "#..#"]
            let result = pixelsOn input
            result `shouldBe` 4
    describe "assembleGrids" $ do
        it "assembles" $ do
            let split = [Grid ["#.", ".."], Grid [".#", ".."], Grid ["..", "#."], Grid ["..", ".#"]]
            let expected = Grid ["#..#", "....", "....", "#..#"]
            let result = assembleGrids split
            result `shouldBe` expected
    describe "run" $ do
        it "works for the example" $ do
            let lines = ["../.# => ##./#../...", ".#./..#/### => #..#/..../..../#..#"]
            let res = run lines 2
            res `shouldBe` 12