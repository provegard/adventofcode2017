module Redist where
import Data.List
import Data.Maybe
import Debug.Trace
import qualified Data.Set as Set

list_of_ones len = take len (repeat 1)

update_list :: [Int] -> Int -> (Int -> Int) -> [Int]
update_list list idx upd = do
  let tup = splitAt idx list
  --let tup2 = (trace (show tup) tup)
  let tup2 = tup
  (fst tup2) ++ [upd (head (snd tup2))] ++ (tail_or_empty (snd tup2))
  where
    tail_or_empty xs = if (length xs == 0) then [] else (tail xs)

next_idx banks idx = if idx == (length banks - 1) then 0 else idx + 1

distrib :: [Int] -> Int -> [Int] -> [Int]
distrib banks idx [] = banks
distrib banks idx to_distribute = do
  distrib (update_list banks idx (+1)) (next_idx banks idx) (tail to_distribute)

balance :: [Int] -> [Int]
balance banks = do
  let _max = maximum banks
  let idx_of_max = fromMaybe 0 (elemIndex _max banks)
  let to_distribute = list_of_ones _max
  let new_banks = update_list banks idx_of_max (*0)
  distrib new_banks (next_idx new_banks idx_of_max) to_distribute

detect_loop_rec :: [Int] -> (Set.Set [Int]) -> Int -> Int
detect_loop_rec banks set counter = do
  if (Set.member banks set) then counter else (detect_loop_rec (balance banks) (Set.insert banks set) (counter + 1))

detect_loop banks = detect_loop_rec banks Set.empty 0

