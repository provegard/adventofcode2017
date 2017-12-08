module Main where
import Registers
import qualified Data.Map.Strict as Map

main = do
  contents <- getContents
  let instructions = map parse_instruction (lines contents)
  let register_map = apply_instructions instructions Map.empty
  let mx = max_register_value register_map
  putStrLn (show mx)
