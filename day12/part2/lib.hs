module Lib where
import Data.Graph

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
bounds theEdges = let allV = map fst theEdges ++ map snd theEdges
                  in (minimum allV, maximum allV)

parse :: [String] -> Graph
parse theLines = let allEdges = concatMap findEdges theLines
                 in buildG (bounds allEdges) allEdges

numberOfGroups :: [String] -> Int
numberOfGroups theLines = length $ components $ parse theLines