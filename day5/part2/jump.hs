module Jump where
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.STRef


run_program :: (STUArray s Int Int) -> Int -> ST s Int
run_program arr len = do
  idx <- newSTRef 0
  steps <- newSTRef 0
  let loop = do
      theIdx <- readSTRef idx
      offset <- readArray arr theIdx
      writeArray arr theIdx (inc_offset offset)
      modifySTRef idx (+offset)
      modifySTRef steps (+1)
      newIdx <- readSTRef idx
      -- loop while our index is within the array
      when (newIdx >= 0 && newIdx < len) loop

  loop -- start first iteration
  result <- readSTRef steps
  return result
  where
    inc_offset offs = if offs >= 3 then offs - 1 else offs + 1

count_steps :: [Int] -> Int
count_steps instructions = do
  let len = length instructions
  runST $ do
    arr <- thaw (listArray (0, len) instructions :: UArray Int Int)
    steps <- run_program arr len
    return steps

