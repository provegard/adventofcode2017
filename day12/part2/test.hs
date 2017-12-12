module Lib.Tests where
import Test.Hspec
import Lib

str = "0 <-> 2\n\
\1 <-> 1\n\
\2 <-> 0, 3, 4\n\
\3 <-> 2, 4\n\
\4 <-> 2, 3, 6\n\
\5 <-> 6\n\
\6 <-> 4, 5\n"

main = hspec $ do
    -- describe "parse" $ do
    --     it "0 <-> 2" $ do
    --         (lookup 0 $ parse ["0 <-> 2"]) `shouldBe` Just [2]
    --     it "0 <-> 2 (reverse)" $ do
    --         (lookup 2 $ parse ["0 <-> 2"]) `shouldBe` Just [0]
    --     it "2 <-> 0, 3, 4" $ do
    --         (lookup 2 $ parse ["2 <-> 0, 3, 4"]) `shouldBe` Just [0, 3, 4]
                
    describe "groupSize" $ do
        it "for example input" $ do
            (groupSize 0 (lines str)) `shouldBe` (6 :: Int)
    --     it "ne,ne,sw,sw" $ do
    --         (stepDistance ["ne", "ne", "sw", "sw"]) `shouldBe` (0 :: Int)
    --     it "ne,ne,s,s" $ do
    --         (stepDistance ["ne", "ne", "s", "s"]) `shouldBe` (2 :: Int)
    --     it "se,sw,se,sw,sw" $ do
    --         (stepDistance ["se", "sw", "se", "sw", "sw"]) `shouldBe` (3 :: Int)
            