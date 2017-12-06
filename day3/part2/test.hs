module Memory.Tests where
import Test.HUnit
import Memory
import qualified Data.Map as Map


tests = test [
  "coords layer 1" ~: "1" ~: [(0,0)] ~=? (square_coords_cw 1),
  "coords layer 2" ~: "2" ~: [(1,0),(1,-1),(0,-1),(-1,-1),(-1,0),(-1,1),(0,1),(1,1)] ~=? (square_coords_cw 2),
  "value_at" ~: "empty mem" ~: 0 ~=? (value_at (0,0) Map.empty),
  "value_at" ~: "non-empty mem" ~: 3 ~=? (value_at (0,0) (Map.insert (0,0) 3 $ Map.empty)),
  "calc_value" ~: "simple" ~: 1 ~=? (calc_value (1,0) (Map.fromList [((0,0),1)])),
  "calc_value" ~: "simple" ~: 1 ~=? (calc_value (-1,1) (Map.fromList [((0,0),1)])),
  "find_value_larger_than" ~: "1->2" ~: 2 ~=? (find_value_larger_than 1),
  "find_value_larger_than" ~: "1->2" ~: 747 ~=? (find_value_larger_than 740)
  ]

main = runTestTT tests

