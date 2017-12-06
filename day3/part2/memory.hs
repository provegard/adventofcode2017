module Memory where
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.List as List

-- square count is the diameter
layer_square_count n = 4 * (2 * n - 2)

-- find next coord given x, y and side length
next :: Int -> Int -> Int -> (Int,Int)
next x y side
  | y == side && x > 1 = (x-1,y)
  | y == 1 && x < side = (x+1,y)
  | x == 1             = (x,y-1)
  | x == side          = (x,y+1)
  | otherwise          = error ("no next for x = " ++ (show x) ++ ", y = " ++ (show y))

-- tuple version of 'next'
nextTup xy side = next (fst xy) (snd xy) side
      
-- returns square coordinates for layer n, in counter-clockwise order
square_coords :: Int -> [(Int, Int)]
square_coords n = do
  let ls = 2 * n - 1
  let list = [1..(layer_square_count n)-1]
  let result = foldl (\ xx _ -> xx ++ [nextTup (last xx) ls] ) [(ls,ls)] list
  map transposed result
  where
    transposed coord = ((fst coord) - n, (snd coord) - n)

-- clockwise version of 'square_coords'
square_coords_cw n = reverse $ square_coords n

-- returns the value at a given coordinate in memory
value_at coord memory = fromMaybe 0 (Map.lookup coord memory)

transpose coord t = ((fst coord) + (fst t), (snd coord) + (snd t))

-- calculate coord value as sum of surrounding squares
calc_value coord memory = value_at (transpose coord (1,0)) memory +
    value_at (transpose coord (1,-1)) memory +
    value_at (transpose coord (0,-1)) memory +
    value_at (transpose coord (-1,-1)) memory +
    value_at (transpose coord (-1,0)) memory +
    value_at (transpose coord (-1,1)) memory +
    value_at (transpose coord (0,1)) memory +
    value_at (transpose coord (1,1)) memory

-- create new memory by calculating value for coord given existing memory
new_memory_with_coord coord memory = do
  let v = calc_value coord memory
  Map.insert coord v $ memory

mem_for_layer 1 _ = Map.fromList [((0,0),1)]
mem_for_layer n prev_mem = do
  -- get coordinates for the new layer
  let coords = square_coords_cw n
  -- create a new memory by folding over the squares
  let new_mem = foldl (\ mem c -> new_memory_with_coord c mem) prev_mem coords
  new_mem

find_value_larger_than_rec :: Int -> Int -> Map.Map (Int,Int) Int -> Int
find_value_larger_than_rec 10 _ _ = error "oops"
find_value_larger_than_rec layerNo v memory = do
  let mem = mem_for_layer layerNo memory
  let mem_values = map snd $ Map.toList mem
  let gt_than = filter (>v) mem_values
  let sorted = List.sort gt_than
  head_or_rec sorted mem
  where
    head_or_rec [] mem = find_value_larger_than_rec (layerNo+1) v mem
    head_or_rec list mem = head list


find_value_larger_than v = find_value_larger_than_rec 1 v Map.empty

