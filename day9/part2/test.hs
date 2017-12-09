module Lib.Tests where
import Test.HUnit
import Lib

t1 = TestCase (assertEqual "empty garbage" ("", 0) (dropGarbage "<>"))
t2 = TestCase (assertEqual "garbage containing random chars" ("", 17) (dropGarbage "<random characters>"))
t3 = TestCase (assertEqual "extra <" ("", 3) (dropGarbage "<<<<>"))
t4 = TestCase (assertEqual "escaped >" ("", 2) (dropGarbage "<{!>}>"))
t5 = TestCase (assertEqual "escaped !" ("", 0) (dropGarbage "<!!>"))
t6 = TestCase (assertEqual "escaped ! and >" ("", 0) (dropGarbage "<!!!>>"))
t7 = TestCase (assertEqual "stuff" ("", 10) (dropGarbage "<{o\"i!a,<{i<a>"))
t8 = TestCase (assertEqual "garbage with trailing" ("{}", 0) (dropGarbage "<>{}"))

t14 = TestCase (assertEqual "group with garbage and nested group" 2 (countGarbage "{<a>,{},<b>}"))
t15 = TestCase (assertEqual "nested garbage" 5 (countGarbage "{<a>,{{},<b!!>,<foo>}}"))

tests = test [t1, t2, t3, t4, t5, t6, t7, t8, t14, t15]

main = runTestTT tests

