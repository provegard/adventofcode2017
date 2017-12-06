module Main where
import Passphrase
import Data.List.Split

non_empty x = (length x) > 0
split_line line = filter non_empty (splitOneOf " \t" line)

main = do
  contents <- getContents
  let count = count_valid (lines contents)
  putStrLn (show count)

