module Main where
import Captcha

main = do
  str <- getLine
  putStrLn $ show (digit_sum str)
