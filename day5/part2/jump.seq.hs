module Jump where
import qualified Data.Sequence as Seq

data Program = Program (Seq.Seq Int) Int Int

get_step_count :: Program -> Int
get_step_count (Program _ _ sc) = sc

is_done :: Program -> Bool
is_done (Program instructions index _) = index < 0 || index >= (length instructions)

jump :: Program -> Program
jump (Program instructions index step_count) =
  --let parts = Seq.splitAt index instructions
  --let jump_offset = head (snd parts)
  --let new_offset = inc_offset jump_offset
  case Seq.lookup index instructions of
    Nothing -> error "unknown"
    Just offset -> Program (new_instr offset) (index + offset) (step_count + 1)
  where
    inc_offset offs
      | offs >= 3  = offs - 1
      | otherwise  = offs + 1
    new_instr offs = Seq.update index (inc_offset offs) instructions

jump_until_done :: Program -> Program
--jump_until_done program
--  | is_done program = program
--  | otherwise       = jump_until_done $! jump program
--jump_until_done program = if (is_done program) then program else jump_until_done $! jump program
jump_until_done program = case (is_done program) of
  True -> program
  False -> jump_until_done $! jump program

count_steps :: [Int] -> Int
count_steps instructions = get_step_count $! jump_until_done (Program (Seq.fromList instructions) 0 0)
