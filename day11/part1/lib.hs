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

max3 a b c = max (max a b) c

manhattan :: CubeCoordinate -> CubeCoordinate -> Int
manhattan a b = max3 (abs (ccX a - ccX b)) (abs (ccY a - ccY b)) (abs (ccZ a - ccZ b))
-- stepsRequired :: [String] -> [String]
-- stepsRequired stepsOut = do
--     let start = CubeCoordinate 0 0 0
--     let childCoord = foldl move start stepsOut
--     []
stepDistance :: [String] -> Int
stepDistance stepsOut = do
    let start = CubeCoordinate 0 0 0
    let childCoord = foldl move start stepsOut
    manhattan childCoord start