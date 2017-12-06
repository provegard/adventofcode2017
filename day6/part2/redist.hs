module Redist where
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

type Banks = Seq.Seq Int

next_idx banks idx = (idx + 1) `mod` (length banks)

-- distribute 'to_distribute' blocks over the memory banks starting at 'idx'
distrib :: Banks -> Int -> Int -> Banks
distrib banks idx 0 = banks
distrib banks idx to_distribute = distrib updated_banks next_idx' (to_distribute - 1)
  where
    updated_banks = Seq.adjust' (+1) idx banks
    next_idx'     = next_idx banks idx

-- run one balance cycle
balance :: Banks -> Banks
balance banks = distrib new_banks (next_idx banks idx_of_max) maxb
  where
    maxb       = maximum banks
    idx_of_max = fromMaybe 0 (Seq.elemIndexL maxb banks)
    new_banks  = Seq.update idx_of_max 0 banks

-- figure out the loop size by recursively balancing until we see a duplicate
-- then return the distance to where we first saw the particular banks configuration
loop_size_rec :: Banks -> (Map.Map Banks Int) -> Int -> Int
loop_size_rec banks theMap counter = do
  case Map.lookup banks theMap of
      Just x  -> counter - x
      Nothing -> loop_size_rec (balance banks) map_with_banks (counter + 1)
  where
    map_with_banks = Map.insert banks counter theMap

-- called from main
loop_size :: [Int] -> Int
loop_size banks_list = loop_size_rec (Seq.fromList banks_list) Map.empty 0
