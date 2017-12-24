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

newtype ComponentMap = ComponentMap (Map.Map Int [Component])

createComponentMap :: [Component] -> ComponentMap
createComponentMap components = do
    let prefixed = concatMap (\c@(Component p1 p2) -> [(p1, c), (p2, c)]) components
    ComponentMap $ sortAndGroup prefixed
    where
        sortAndGroup assocs = Map.fromListWith (++) [(k, [v]) | (k, v) <- assocs]

findMatchingComponents :: Component -> ComponentMap -> [Component]
findMatchingComponents c@(Component a b) (ComponentMap cmap) = do
    let al = fromMaybe [] $ Map.lookup a cmap
    let bl = fromMaybe [] $ Map.lookup b cmap
    delete c (nub (al ++ bl))

findStartComponents :: ComponentMap -> [Component]
findStartComponents (ComponentMap cm) = fromMaybe [] $ Map.lookup 0 cm

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

build' :: ComponentMap -> [[Component]]
build' compMap = do
    -- let (starters, rest) = partition isStarter components
    -- let restSet = Set.fromList rest
    let starters = findStartComponents compMap
    concatMap (\s -> buildFrom (Set.singleton s) [s] s) starters
    where
        isStarter (Component a b) = a == 0 || b == 0
        buildFrom :: Set.Set Component -> [Component] -> Component -> [[Component]]
        buildFrom !used !previousList previous = do
            -- a connector is ok if it fits with the previous and hasn't been used
            let matching = findMatchingComponents previous compMap
            let candidates = filter (\c -> not (Set.member c used) && not (isStarter c)) matching
            if null candidates then [previousList] else concatMap (\c -> do
                    --let compList = previousList ++ [c]
                    let compList = c : previousList -- prepend is faster I think, and order doesn't matter
                    buildFrom (Set.insert c used) compList c) candidates

build :: [Component] -> [Bridge]
build components = map Bridge $ build' (createComponentMap components)

findStrongest :: [String] -> (Int, Bridge)
findStrongest strings = do
    let components = createComponents strings
    let bridges = build components
    let b = maximumBy (\a b -> compare (strength a) (strength b)) bridges
    (strength b, b)