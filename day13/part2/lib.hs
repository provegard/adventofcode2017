module Lib where
import Data.Foldable
import Data.Maybe
import Data.List.Split

toInt :: String -> Int
toInt x = read x :: Int

-- stripTrailingComma :: String -> String
-- stripTrailingComma = rstrip
--   where
--     lstrip = dropWhile (`elem` ",")
--     rstrip = reverse . lstrip . reverse

--data Layer = Layer { depth :: Int, scannerRange :: Int, scannerPosition :: Int }
data Layer = Layer { scannerRange :: Int, scannerPosition :: Int, scannerDirection :: Int } deriving (Show, Eq)

type Layers = [(Int, Layer)]

data World = World { worldLayers :: Layers, packetPos :: Int, currentSeverity :: Int }

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
    where
        createLayer line = do
            let (d:r:_) = splitOn ":" line
            (toInt d, newLayer $ toInt r)

maxDepth :: Layers -> Int
maxDepth layers = maximum $ map fst layers

-- Advance the world without moving packet pos or updating severity
advanceWorldOnly :: World -> World
advanceWorldOnly world@(World layers packetPos currentSeverity)
    | packetPos > maxDepth layers = world
    | otherwise                   = do
        let newLayers = map moveScannerTup layers
        World newLayers packetPos currentSeverity
        where
            moveScannerTup (depth, layer) = (depth, moveScanner layer)

tick :: Time -> World -> World
tick time world@(World layers packetPos currentSeverity)
    | packetPos > maxDepth layers = world
    | otherwise                   = do
        let newPos = packetPos + 1
        let newSeverity = calcSeverity newPos + currentSeverity
        let movedWorld = advanceWorldOnly world
        --let newWorld = World newLayers newPos newSeverity
        let newWorld = World (worldLayers movedWorld) newPos newSeverity
        tick (time + 1) newWorld
        where
            calcSeverity aPos = case lookup aPos layers of
                Just l  | scannerPosition l == 0 -> 1 -- arbitrary > 0
                _                                -> 0

-- getSeverity :: Layers -> Int -> Int
-- getSeverity layers delay = do
--     let world = World layers (-1) 0
--     let resultingWorld = tick 0 delay world
--     currentSeverity resultingWorld

findDelay :: World -> Int -> Int
--findDelay layers delay = if getSeverity layers delay == 0 then delay else findDelay layers (delay + 1)
findDelay world delay = do
    let whatIfWorld = tick 0 world
    if currentSeverity whatIfWorld == 0 then
        -- found it
        delay
    else
        findDelay (advanceWorldOnly world) (delay + 1)

getMinDelay :: [String] -> Int
getMinDelay lines = do
    let layers = readLayers lines
    let baseWorld = World layers (-1) 0
    findDelay baseWorld 0