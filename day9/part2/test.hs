module Lib.Tests where
import Test.HUnit
import Lib
import qualified Data.Map as Map

t1 = TestCase (assertEqual "empty garbage" ("", 0) (dropGarbage "<>"))
t2 = TestCase (assertEqual "garbage containing random chars" ("", 17) (dropGarbage "<random characters>"))
t3 = TestCase (assertEqual "extra <" ("", 3) (dropGarbage "<<<<>"))
t4 = TestCase (assertEqual "escaped >" ("", 2) (dropGarbage "<{!>}>"))
t5 = TestCase (assertEqual "escaped !" ("", 0) (dropGarbage "<!!>"))
t6 = TestCase (assertEqual "escaped ! and >" ("", 0) (dropGarbage "<!!!>>"))
t7 = TestCase (assertEqual "stuff" ("", 10) (dropGarbage "<{o\"i!a,<{i<a>"))
t8 = TestCase (assertEqual "garbage with trailing" ("{}", 0) (dropGarbage "<>{}"))

t9 = TestCase (assertEqual "single group" [Group 1] (findGroups "{}"))
t10 = TestCase (assertEqual "nested group" [Group 1, Group 2] (findGroups "{{}}"))
t11 = TestCase (assertEqual "group with garbage" [Group 1] (findGroups "{<a>,<b>}"))
t12 = TestCase (assertEqual "group with garbage and nested group" [Group 1, Group 2] (findGroups "{<a>,{},<b>}"))
t13 = TestCase (assertEqual "two nested groups" [Group 1, Group 2, Group 2] (findGroups "{{},{}}"))

t14 = TestCase (assertEqual "group with garbage and nested group" 2 (countGarbage "{<a>,{},<b>}"))
t15 = TestCase (assertEqual "nested garbage" 5 (countGarbage "{<a>,{{},<b!!>,<foo>}}"))

tests = test [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15]

main = runTestTT tests

