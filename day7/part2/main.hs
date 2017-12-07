module Main where
import Towers


main = do
  contents <- getContents
  let input_lines = map parse_line (lines contents)
  let tree = make_tree input_lines
  case find_changed_weight tree (-1) of
      Just w  -> putStrLn (show w)
      Nothing -> putStrLn ":("
