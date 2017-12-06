module Captcha.Tests where
import Test.HUnit
import Captcha
--import Control.Exception (ErrorCall(ErrorCall), evaluate, Exception, handleJust)
--import Control.Exception (Exception, evaluate)
--import Control.Monad

--instance Eq ErrorCall where
--  x == y = (show x) == (show y)

--assertException :: (Exception e, Eq e) => e -> IO a -> IO ()
--assertException ex action =
--  handleJust isWanted (const $ return ()) $ do
--    action
--    assertFailure $ "Expected exception: " ++ show ex
--  where isWanted = guard . (== ex) 

--assertError ex f = assertException (ErrorCall ex) $ evaluate f

test1 = TestCase (assertEqual "equal digits with wrap around" 2 (digit_sum "11"))

test2 = TestCase (assertEqual "different digits" 0 (digit_sum "12"))

test3 = TestCase (assertEqual "equal digits in the middle" 1 (digit_sum "3114"))

test4 = TestCase (assertEqual "1122" 3 (digit_sum "1122"))

test5 = TestCase (assertEqual "1111" 4 (digit_sum "1111"))

test6 = TestCase (assertEqual "1234" 0 (digit_sum "1234"))

test7 = TestCase (assertEqual "91212129" 9 (digit_sum "91212129"))

--test8 = TestCase (assertError "not a digit: 'a'" (evaluate $ digit_sum "a"))

tests = TestList [
  TestLabel "test1" test1,
  TestLabel "test2" test2,
  TestLabel "test3" test3,
  TestLabel "test4" test4,
  TestLabel "test5" test5,
  TestLabel "test6" test6,
  TestLabel "test7" test7
  --TestLabel "test8" test8
  ]

main = runTestTT tests

