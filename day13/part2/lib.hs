{-# LANGUAGE BangPatterns #-}
module Lib where
import Data.Foldable
import Data.Maybe
import Data.List.Split

toInt :: String -> Int
toInt x = read x :: Int

data Layer = Layer { scannerRange :: Int, scannerPosition :: Int, scannerDirection :: Int } deriving (Show, Eq)

type Layers = [(Int, Layer)]

newLayer :: Int -> Layer
newLayer range = Layer range 0 1

moveScanner :: Layer -> Layer
moveScanner (Layer sr sp sd)
    | sp == 0 && sd == -1       = Layer sr 1 1
    | sp == (sr - 1) && sd == 1 = Layer sr (sr - 2) (-1)
    | otherwise                 = Layer sr (sp + sd) sd

readLayers :: [String] -> Layers
readLayers = map createLayer
    where createLayer line = let (d:r:_) = splitOn ":" line
                             in (toInt d, newLayer $ toInt r)

maxDepthOf :: Layers -> Int
maxDepthOf layers = maximum $ map fst layers

advanceLayers :: Layers -> Layers
advanceLayers = map moveScannerTup
    where moveScannerTup (depth, !layer) = (depth, moveScanner layer)

-- generate an infinite sequence of layers (layer configurations)
layersGenerator :: Layers -> [Layers]
layersGenerator layers = layers : (layersGenerator $! advanceLayers layers)

isCaughtIn :: [Layers] -> Int -> Bool
isCaughtIn layerList maxDepth = isCaught layerList (-1)
    where
        isCaught :: [Layers] -> Int -> Bool
        isCaught (layers:remaining) pos
            | pos > maxDepth = False
            | otherwise = do
                let newPos = pos + 1
                let caught = checkCaught newPos
                caught || isCaught remaining newPos
                where
                    checkCaught aPos = case lookup aPos layers of
                        Just l | scannerPosition l == 0 -> True
                        _                               -> False

findDelay :: [Layers] -> Int -> Int -> Int
findDelay layersList delay maxDepth = do
    let passed = not $! isCaughtIn layersList maxDepth
    if passed then delay else next
    where next = findDelay (tail layersList) (delay + 1) maxDepth

getMinDelay :: [String] -> Int
getMinDelay lines = do
    let layers = readLayers lines
    let layersList = layersGenerator layers
    findDelay layersList 0 (maxDepthOf layers)