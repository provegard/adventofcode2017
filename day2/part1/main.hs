module Main where
import Checksum
import Data.List.Split

non_empty x = (length x) > 0
split_line line = filter non_empty (splitOneOf " \t" line)

main = do
  contents <- getContents
  let table = map split_line (lines contents)
  --putStrLn $ show table
  let cs = checksum table
  putStrLn (show cs)

