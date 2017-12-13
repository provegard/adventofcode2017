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

data World = World { layers :: Layers, packetPos :: Int, currentSeverity :: Int }

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

tick :: Time -> World -> World
tick time world@(World layers packetPos currentSeverity)
    | packetPos > maxDepth layers = world
    | otherwise                   = do
        let newPos = packetPos + 1
        let newSeverity = calcSeverity newPos + currentSeverity
        let newLayers = map moveScannerTup layers
        let newWorld = World newLayers newPos newSeverity
        tick (time + 1) newWorld
        where
            moveScannerTup (depth, layer) = (depth, moveScanner layer)
            calcSeverity aPos = case lookup aPos layers of
                Just l  | scannerPosition l == 0 -> aPos * scannerRange l
                _                               -> 0

getSeverity :: [String] -> Int
getSeverity lines = do
    let layers = readLayers lines
    let world = World layers (-1) 0
    let resultingWorld = tick 0 world
    currentSeverity resultingWorld