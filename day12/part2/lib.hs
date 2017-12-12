module Lib where
import Data.Foldable
import Data.Graph
import Data.Maybe

toInt :: String -> Int
toInt x = read x :: Int

stripTrailingComma :: String -> String
stripTrailingComma = rstrip
  where
    lstrip = dropWhile (`elem` ",")
    rstrip = reverse . lstrip . reverse

findEdges :: String -> [(Vertex, Vertex)]
findEdges line = do
    let (x1:_:rest) = words line
    let restNum = map (toInt . stripTrailingComma) rest
    let x1Num = toInt x1
    map (\a -> (a, x1Num)) restNum

bounds :: [(Vertex, Vertex)] -> (Vertex, Vertex)
bounds theEdges = do
    let allV = map fst theEdges ++ map snd theEdges
    (minimum allV, maximum allV)

parse :: [String] -> Graph
parse theLines = let allEdges = concatMap findEdges theLines
                 in buildG (bounds allEdges) allEdges

-- groupSize :: Int -> [String] -> Int
-- groupSize node theLines = do
--     let g = parse theLines
--     length $ reachable g node

numberOfGroups :: [String] -> Int
numberOfGroups theLines = let g = parse theLines
                          in length $ components g