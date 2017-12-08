module Main where
import Registers
import qualified Data.Map.Strict as Map

main = do
  contents <- getContents
  let instructions = map parse_instruction (lines contents)
  let registers = apply_instructions instructions
  let mx = max_held registers
  putStrLn (show mx)
