module Lib.Tests where
import Test.HUnit
import Lib

t1 = TestCase (assertEqual "empty garbage" "" (dropGarbage "<>"))
t2 = TestCase (assertEqual "garbage containing random chars" "" (dropGarbage "<random characters>"))
t3 = TestCase (assertEqual "extra <" "" (dropGarbage "<<<<>"))
t4 = TestCase (assertEqual "escaped >" "" (dropGarbage "<{!>}>"))
t5 = TestCase (assertEqual "escaped !" "" (dropGarbage "<!!>"))
t6 = TestCase (assertEqual "escaped ! and >" "" (dropGarbage "<!!!>>"))
t7 = TestCase (assertEqual "stuff" "" (dropGarbage "<{o\"i!a,<{i<a>"))
t8 = TestCase (assertEqual "garbage with trailing" "{}" (dropGarbage "<>{}"))

t9 = TestCase (assertEqual "single group" (1) (findScore "{}"))
t10 = TestCase (assertEqual "single group" (1 + 2) (findScore "{{}}"))
t11 = TestCase (assertEqual "single group" (1) (findScore "{<a>,<b>}"))
t12 = TestCase (assertEqual "single group" (1 + 2) (findScore "{<a>,<b>,{}}"))
t13 = TestCase (assertEqual "single group" (1 + 2 + 2) (findScore "{{},{}}"))


tests = test [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13]

main = runTestTT tests

