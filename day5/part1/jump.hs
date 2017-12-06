module Jump where

data Program = Program [Int] Int Int

get_step_count :: Program -> Int
get_step_count (Program _ _ sc) = sc

is_done :: Program -> Bool
is_done (Program instructions index _) = index < 0 || index >= (length instructions)

jump :: Program -> Program
jump (Program instructions index step_count) = do
  let parts = splitAt index instructions
  let jump_offset = head $ snd parts
  let new_offset = jump_offset + 1
  let new_list = (fst parts) ++ [new_offset] ++ (tail $ snd parts)
  Program new_list (index + jump_offset) (step_count + 1)

jump_until_done :: Program -> Program
jump_until_done program
  | is_done program = program
  | otherwise       = jump_until_done $ jump program

count_steps :: [Int] -> Int
count_steps instructions = get_step_count $ jump_until_done (Program instructions 0 0)
