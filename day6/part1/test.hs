module Memory.Tests where
import Test.HUnit
import Redist
import qualified Data.Map as Map


tests = test [
  "balance 1" ~: "1" ~: [2, 4, 1, 2] ~=? (balance [0, 2, 7, 0]),
  "balance 1" ~: "2" ~: [3, 1, 2, 3] ~=? (balance [2, 4, 1, 2]),
  "balance 1" ~: "3" ~: [0, 2, 3, 4] ~=? (balance [3, 1, 2, 3]),
  "balance 1" ~: "4" ~: [1, 3, 4, 1] ~=? (balance [0, 2, 3, 4]),
  "balance 1" ~: "5" ~: [2, 4, 1, 2] ~=? (balance [1, 3, 4, 1]),
  "loop" ~: "1" ~: 5 ~=? (detect_loop [0, 2, 7, 0])
  ]

main = runTestTT tests

