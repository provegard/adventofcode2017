module Captcha.Tests where
import Test.HUnit
import Captcha


tests = TestList [
  "test1" ~: 6 ~=? (digit_sum "1212"),
  "test2" ~: 0 ~=? (digit_sum "1221"),
  "test3" ~: 4 ~=? (digit_sum "123425"),
  "test4" ~: 12 ~=? (digit_sum "123123"),
  "test5" ~: 4 ~=? (digit_sum "12131415")
  ]

main = runTestTT tests

