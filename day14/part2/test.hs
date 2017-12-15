module Lib.Tests where
import Test.Hspec
import Lib

main = hspec $ do
    describe "findConnectedNodes" $ do
        it "finds a single node" $ do
            (findConnectedNodes (RowNodes [0]) Nothing) `shouldBe` [(ConnectedNode 0 [])]
        it "finds connected nodes" $ do
            (findConnectedNodes (RowNodes [0, 1]) Nothing) `shouldBe` [(ConnectedNode 0 [1]), (ConnectedNode 1 [])]
        it "finds separated nodes" $ do
            (findConnectedNodes (RowNodes [0, 2]) Nothing) `shouldBe` [(ConnectedNode 0 []), (ConnectedNode 2 [])]
        it "finds in a longer list" $ do
            (findConnectedNodes (RowNodes [0, 1, 2]) Nothing) `shouldBe` [(ConnectedNode 0 [1]), (ConnectedNode 1 [2]), (ConnectedNode 2 [])]
        it "finds connected nodes across rows" $ do
            (findConnectedNodes (RowNodes [0]) (Just (RowNodes [128]))) `shouldBe` [(ConnectedNode 0 [128])]
        it "ignores non-connected nodes across rows" $ do
            (findConnectedNodes (RowNodes [0]) (Just (RowNodes [129]))) `shouldBe` [(ConnectedNode 0 [])]
        it "merges nodes" $ do
            (findConnectedNodes (RowNodes [0, 1]) (Just (RowNodes [128]))) `shouldBe` [(ConnectedNode 0 [1, 128]), (ConnectedNode 1 [])]
    describe "countRegions" $ do
        it "works for the test input" $ do
            (countRegions "flqrgnkx") `shouldBe` (1242 :: Int)