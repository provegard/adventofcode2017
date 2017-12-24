{-# LANGUAGE BangPatterns #-}
module Lib where
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map
import Data.Char
import Data.Maybe
import Data.List.Split
import Data.List
import qualified Data.Set as Set

data Component = Component Int Int deriving (Eq, Show, Ord)

newtype Bridge = Bridge [Component] deriving (Eq, Show)

strength :: Bridge -> Int
strength (Bridge ports) = let compStrength (Component a b) = a + b
                          in sum $ map compStrength ports 

toInt :: String -> Int
toInt x = read x :: Int

parseLine :: String -> Component
parseLine str = do
    let (a:b:rest) = splitOn "/" str
    Component (toInt a) (toInt b)

createComponents :: [String] -> [Component]
createComponents = map parseLine

fitTogether :: Component -> Component -> Bool
fitTogether (Component p1a p1b) (Component p2a p2b) = p1a == p2a || p1a == p2b || p1b == p2a || p1b == p2b

build' :: [Component] -> [[Component]]
build' !components = do
    let (starters, rest) = partition isStarter components
    let restSet = Set.fromList rest
    --concatMap (\s -> [s] : buildFrom (Set.singleton s) s rest) starters
    concatMap (\s -> [s] : buildFrom (Set.singleton s) [s] s (Set.delete s restSet)) starters
    where
        isStarter (Component a b) = a == 0 || b == 0
        buildFrom :: Set.Set Component -> [Component] -> Component -> Set.Set Component -> [[Component]]
        buildFrom !used !previousList previous !othersSet = do
            -- a connector is ok if it fits with the previous and hasn't been used
            --let others = Set.toList othersSet
            let candidateSet = Set.filter (\c -> fitTogether previous c && not (Set.member c used)) othersSet
            let candidates = Set.toList candidateSet
            concatMap (\c -> do
                let compList = previousList ++ [c]
                compList : buildFrom (Set.insert c used) compList c (Set.delete c othersSet)) candidates

build :: [Component] -> [Bridge]
build components = map Bridge $ build' components

findStrongest :: [String] -> (Int, Bridge)
findStrongest strings = do
    let components = createComponents strings
    let bridges = build components
    let b = maximumBy (\a b -> compare (strength a) (strength b)) bridges
    -- let strengths = map strength bridges
    -- maximum strengths
    (strength b, b)