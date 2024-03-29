module Lib where
import Data.Foldable
import Data.Maybe
import Data.List.Split

toInt :: String -> Int
toInt x = read x :: Int

newtype Layer = Layer { scannerRange :: Int } deriving (Show, Eq)

type Layers = [(Int, Layer)]

scannerAt0 :: Layer -> Int -> Bool
scannerAt0 (Layer sr) time = 0 == time `mod` (2 * sr - 2)

readLayers :: [String] -> Layers
readLayers = map createLayer
    where createLayer line = let (d:r:_) = splitOn ":" line
                             in (toInt d, Layer $ toInt r)

maxDepthOf :: Layers -> Int
maxDepthOf layers = maximum $ map fst layers

isCaughtIn :: Layers -> Int -> Int -> Bool
isCaughtIn layers delay maxDepth = isCaught delay (-1)
    where
        isCaught :: Int -> Int -> Bool
        isCaught delay pos | pos > maxDepth = False
        isCaught delay pos = do
            let newPos = pos + 1
            let caught = checkCaught newPos delay
            caught || isCaught (delay + 1) newPos
        checkCaught aPos aDelay = maybe False (`scannerAt0` aDelay) (lookup aPos layers)

findDelay :: Layers -> Int -> Int -> Int
findDelay layers delay maxDepth = do
    let passed = not $ isCaughtIn layers delay maxDepth
    if passed then delay else next
    where next = findDelay layers (delay + 1) maxDepth

getMinDelay :: [String] -> Int
getMinDelay lines = do
    let layers = readLayers lines
    findDelay layers 0 (maxDepthOf layers)