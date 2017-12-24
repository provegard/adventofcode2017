{-# LANGUAGE BangPatterns #-}
module Lib where
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map
import Data.Char
import Data.Maybe
import Data.List.Split
import Data.List
import qualified Data.Set as Set
import qualified Data.Graph as G
import qualified Data.Tree as T
import Debug.Trace

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

isValidBridge :: Bridge -> Bool
isValidBridge (Bridge (x:components)) = validConnections (otherPort x 0) components
    where
        validConnections _ [] = True
        validConnections n (a:rest) = do
            let (Component a1 a2) = a
            (n == a1 || n == a2) && validConnections (otherPort a n) rest
        otherPort (Component a b) p = if a == p then b else a

build :: [Component] -> [Bridge]
build components = do
    let compMap = createComponentMap components
    let starters = findStartComponents compMap
    let graphInput = map (\c -> do
        let others = filter (not . isStarter) $ findMatchingComponents c compMap
        let otherKeys = map keyOf others
        (c, keyOf c, otherKeys)
        ) components
    let (g, lookupNode, lookupVertex) = G.graphFromEdges graphInput
    let startVertices = mapMaybe (lookupVertex . keyOf) starters
    let forest = G.dfs g startVertices
    filter isValidBridge $ map (`toBridge` lookupNode) forest
    where
        isStarter (Component a b) = a == 0 || b == 0
        keyOf (Component a b) = a * 1024 + b
        toBridge :: T.Tree G.Vertex -> (G.Vertex -> (Component, Int, [Int])) -> Bridge
        toBridge tree lookupNode = do
            let vertices = T.flatten tree
            let nodes = map (\v -> do
                let (c, _, _) = lookupNode v
                c
                ) vertices
            Bridge nodes

findStrongest :: [String] -> (Int, Bridge)
findStrongest strings = do
    let components = createComponents strings
    let bridges = build components
    let b = if null bridges then error "no valid bridges" else maximumBy (\a b -> compare (strength a) (strength b)) bridges
    (strength b, b)