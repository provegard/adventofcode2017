module Lib.Tests where
import Test.Hspec
import Lib
import qualified Data.Sequence as Seq

main = hspec $ do
    describe "parseInputLine" $ do
        it "parses a line" $ do
            let line = "p=<995,1767,1881>, v=<141,252,264>, a=<-9,-12,-14>"
            let part = parseInputLine line
            part `shouldBe` (Particle (Triple 995 1767 1881) (Triple 141 252 264) (Triple (-9) (-12) (-14)))
    describe "leftStanding" $ do
        it "works for the example" $ do
            let p1 = "p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>"
            let p2 = "p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>"
            let p3 = "p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>"
            let p4 = "p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>"
            let lines = [p1, p2, p3, p4]
            let res = leftStanding lines 10
            res `shouldBe` [3]