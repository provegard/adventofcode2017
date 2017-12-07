module Towers.Tests where
import Test.HUnit
import Towers


tests = test [
  "parse_line" ~: "name + weight" ~:
    (InputLine "pbga" 66 []) ~=? (parse_line "pbga (66)"),
  "parse_line" ~: "name + weight + subs" ~:
    (InputLine "fwft" 72 ["ktlj", "cntj", "xhth"]) ~=? (parse_line "fwft (72) -> ktlj, cntj, xhth"),
  "make_tree" ~: "only root" ~:
    (TreeNode "root" 10 []) ~=? (make_tree [InputLine "root" 10 []]),
  "make_tree" ~: "one child" ~:
    (TreeNode "root" 10 [TreeNode "child" 5 []]) ~=? (make_tree [(InputLine "child" 5 []), (InputLine "root" 10 ["child"])]),
  "make_tree" ~: "one child, reverse input order" ~:
    (TreeNode "root" 10 [TreeNode "child" 6 []]) ~=? (make_tree [(InputLine "root" 10 ["child"]), (InputLine "child" 6 [])])
  ]

main = runTestTT tests

