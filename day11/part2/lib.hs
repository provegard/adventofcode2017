module Lib where
import Data.Foldable

-- https://www.redblobgames.com/grids/hexagons/

data CubeCoordinate = CubeCoordinate { ccX :: Int, ccY :: Int, ccZ :: Int } deriving (Eq, Show)

move :: CubeCoordinate -> String -> CubeCoordinate
move cc "n" = CubeCoordinate (ccX cc) (ccY cc + 1) (ccZ cc - 1)
move cc "ne" = CubeCoordinate (ccX cc + 1) (ccY cc) (ccZ cc - 1)
move cc "se" = CubeCoordinate (ccX cc + 1) (ccY cc - 1) (ccZ cc)
move cc "s"  = CubeCoordinate (ccX cc) (ccY cc - 1) (ccZ cc + 1)
move cc "sw" = CubeCoordinate (ccX cc - 1) (ccY cc) (ccZ cc + 1)
move cc "nw" = CubeCoordinate (ccX cc - 1) (ccY cc + 1) (ccZ cc)
move _ x     = error ("unknown direction: " ++ x)

max3 :: Int -> Int -> Int -> Int
max3 a b = max (max a b)

manhattan :: CubeCoordinate -> CubeCoordinate -> Int
manhattan a b = max3 (abs (ccX a - ccX b)) (abs (ccY a - ccY b)) (abs (ccZ a - ccZ b))

furthestDistance :: [String] -> Int
furthestDistance stepsOut = do
    let result = foldl track (start, 0) stepsOut
    snd result
    where
        start = CubeCoordinate 0 0 0
        track :: (CubeCoordinate, Int) -> String -> (CubeCoordinate, Int)
        track (cc,m) dir = do
            let newCoord = move cc dir
            let newDist = manhattan start newCoord
            let newMax = max m newDist
            (newCoord, newMax)