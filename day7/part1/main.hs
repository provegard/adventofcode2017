module Main where
import Towers


main = do
  contents <- getContents
  let input_lines = map parse_line (lines contents)
  let tree = make_tree input_lines
  putStrLn (t_name tree)
