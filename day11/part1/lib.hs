module Lib where
import Data.Foldable

-- https://www.redblobgames.com/grids/hexagons/

data CubeCoordinate = CubeCoordinate { ccX :: Int, ccY :: Int, ccZ :: Int } deriving (Eq, Show)

add :: CubeCoordinate -> (Int, Int, Int) -> CubeCoordinate
add cc (x, y, z) = CubeCoordinate (ccX cc + x) (ccY cc + y) (ccZ cc + z)

move :: CubeCoordinate -> String -> CubeCoordinate
move cc "n"  = add cc (0, 1, -1)
move cc "ne" = add cc (1, 0, -1)
move cc "se" = add cc (1, -1, 0)
move cc "s"  = add cc (0, -1, 1)
move cc "sw" = add cc (-1, 0, 1)
move cc "nw" = add cc (-1, 1, 0)
move _ x     = error ("unknown direction: " ++ x)

distance :: CubeCoordinate -> Int
distance (CubeCoordinate x y z) = maximum $ map abs [x, y, z]

stepDistance :: [String] -> Int
stepDistance stepsOut = let start = CubeCoordinate 0 0 0
                        in distance $ foldl move start stepsOut