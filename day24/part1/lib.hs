{-# LANGUAGE BangPatterns #-}
module Lib where
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map
import Data.Char
import Data.Maybe
import Data.List.Split
import Data.List

data Port = Port Int Int deriving (Eq, Show)

newtype Bridge = Bridge [Port]

strength :: Bridge -> Int
strength (Bridge ports) = let portStrength (Port a b) = a + b
                          in sum $ map portStrength ports 

toInt :: String -> Int
toInt x = read x :: Int

parseLine :: String -> Port
parseLine str = do
    let (a:b:rest) = splitOn "/" str
    Port (toInt a) (toInt b)

createPorts :: [String] -> [Port]
createPorts = map parseLine

fitTogether :: Port -> Port -> Bool
fitTogether (Port p1a p1b) (Port p2a p2b) = p1a == p2a || p1a == p2b || p1b == p2a || p1b == p2b

portWithOthers :: [Port] -> [(Port, [Port])]
portWithOthers [] = []
portWithOthers ports = map combo [0..(length ports - 1)]
    where 
        combo idx = do
            let (x,y) = splitAt idx ports
            let next = head y
            let others = x ++ drop 1 y
            (next, others)

build' :: [Port] -> [[Port]]
build' ports = do
    let combos = portWithOthers ports
    -- TODO: Generates duplicate port lists!
    concatMap (\(p, rest) -> [p] : combine p rest) combos
    where
        combine :: Port -> [Port] -> [[Port]]
        combine p others = do
            let (ok, nok) = partition (fitTogether p) others
            -- for each port in 'ok', create a combination
            if null ok then [] else concatMap (\i -> combine' p i ok nok)[0..(length ok - 1)]
        combine' :: Port -> Int -> [Port] -> [Port] -> [[Port]]
        combine' p idx ok nok = do
            let (x,y) = splitAt idx ok
            let next = head y
            let others = x ++ drop 1 y
            [p, next] : combine next (others ++ nok)

build :: [Port] -> [Bridge]
build ports = map Bridge $ build' ports

findStrongest :: [String] -> Int
findStrongest strings = do
    let ports = createPorts strings
    let bridges = build ports
    let strengths = map strength bridges
    maximum strengths