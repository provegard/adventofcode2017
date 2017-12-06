module Memory where

manhattan_distance xy = abs (fst xy) + abs (snd xy)

-- each layer is U * U squares
-- layer 1 is 1 * 1 squares = 1
-- layer 2 is 3 * 3 squares = 9
-- layer 3 is 5 * 5 squares = 25
-- => layer N is (2N-1)*(2N-1) squares = U, bottom right square has no. U
layer_bottom_right n = (2 * n - 1) * (2 * n - 1)

find_layer_rec :: Int -> Int -> Int
find_layer_rec sq_no n = do
  let br = layer_bottom_right n
  if br >= sq_no then n else find_layer_rec sq_no (n + 1)

-- find the layer on which the square resides
-- the layer side is (2n-1) squares
-- the layer "diameter" is 4*(2n-2)
find_layer :: Int -> Int
find_layer sq_no = find_layer_rec sq_no 1

-- square count is the diameter
layer_square_count n = 4 * (2 * n - 2)

next :: Int -> Int -> Int -> (Int,Int)
next x y side
  | y == side && x > 1 = (x-1,y)
  | y == 1 && x < side = (x+1,y)
  | x == 1             = (x,y-1)
  | x == side          = (x,y+1)

nextTup xy side = next (fst xy) (snd xy) side

square_coords :: Int -> [(Int, Int)]
square_coords n = do
  let ls = 2 * n - 1
  let list = [1..(layer_square_count n)-1]
  foldl (\ xx _ -> xx ++ [nextTup (last xx) ls] ) [(ls,ls)] list

coord :: Int -> (Int, Int)
coord square_no = do
  let layer = find_layer square_no
  let br = layer_bottom_right layer
  -- distance (in square number) between bottom-right and input
  let dist = br - square_no
  let coords = square_coords layer
  let coord = head (drop dist coords)
  -- half layer side
  let sub = layer
  let transposed = ((fst coord) - sub, (snd coord) - sub)
  transposed


carry_distance square_no = manhattan_distance (coord square_no)
