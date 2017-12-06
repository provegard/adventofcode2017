module Main where
import Jump

to_int x = read x :: Int

main = do
  contents <- getContents
  let steps = count_steps $ map to_int $ lines contents 
  putStrLn (show steps)

