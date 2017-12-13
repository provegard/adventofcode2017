module Lib where
import Data.Foldable
import Data.Maybe
import Data.List.Split

toInt :: String -> Int
toInt x = read x :: Int

data Layer = Layer { scannerRange :: Int, scannerPosition :: Int, scannerDirection :: Int } deriving (Show, Eq)

type Layers = [(Int, Layer)]

data World = World { worldLayers :: Layers, packetPos :: Int, caught :: Bool, maxDepth :: Int }

type Time = Int

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
    where moveScannerTup (depth, layer) = (depth, moveScanner layer)

-- generate an infinite sequence of layers (layer configurations)
layersGenerator :: Layers -> [Layers]
layersGenerator layers = layers : layersGenerator (advanceLayers layers)

isCaughtIn :: Layers -> Int -> Bool
isCaughtIn layers maxDepth = do
    let world = World layers (-1) False maxDepth
    any caught (generateWorlds world)
    where
        generateWorlds :: World -> [World]
        generateWorlds world@(World layers packetPos _ maxDepth) = world : nextWorlds
            where
                nextWorlds
                    | packetPos > maxDepth = []
                    | otherwise = do
                        let newPos = packetPos + 1
                        let caught = isCaught newPos
                        let newLayers = advanceLayers layers
                        let newWorld = World newLayers newPos caught maxDepth
                        world : generateWorlds newWorld
                    where
                        isCaught aPos = case lookup aPos layers of
                            Just l | scannerPosition l == 0 -> True
                            _                               -> False

findDelay :: [Layers] -> Int -> Int -> Int
findDelay (layers:remaining) delay maxD epth = 
    if not (isCaughtIn layers maxDepth) then delay else findDelay remaining (delay + 1) maxDepth

getMinDelay :: [String] -> Int
getMinDelay lines = do
    let layers = readLayers lines
    let layersList = layersGenerator layers
    findDelay layersList 0 (maxDepthOf layers)