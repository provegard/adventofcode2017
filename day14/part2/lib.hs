module Lib where
import Hash (realHash)
import Data.Graph
import Data.Maybe

-- (row, col), 0-based
--newtype Coord = Coord (Int, Int)
newtype Bits = Bits [Int]
data RowHash = RowHash Int Bits
data RowNodes = RowNodes [(Int, Vertex)]

numBits :: Char -> [Int]
numBits '0' = [0,0,0,0]
numBits '1' = [0,0,0,1]
numBits '2' = [0,0,1,0]
numBits '3' = [0,0,1,1]
numBits '4' = [0,1,0,0]
numBits '5' = [0,1,0,1]
numBits '6' = [0,1,1,0]
numBits '7' = [0,1,1,1]
numBits '8' = [1,0,0,0]
numBits '9' = [1,0,0,1]
numBits 'a' = [1,0,1,0]
numBits 'b' = [1,0,1,1]
numBits 'c' = [1,1,0,0]
numBits 'd' = [1,1,0,1]
numBits 'e' = [1,1,1,0]
numBits 'f' = [1,1,1,1]

allBitsInHash :: String -> [Int]
allBitsInHash = concatMap numBits

rowHashes :: String -> [RowHash]
rowHashes input = let doHash i = realHash (input ++ "-" ++ show i)
                  in map (\i -> RowHash i (Bits (allBitsInHash $ doHash i))) [0..127]

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex = zip [0..]

rowHashToRowNodes :: RowHash -> RowNodes
rowHashToRowNodes (RowHash rowIdx (Bits bits)) = do
    let indexedBits = zipWithIndex bits
    let indexedOnes = filter (\t -> snd t == 1) indexedBits
    RowNodes $ map toNode indexedOnes
    where
        toNode (idx, _) = (idx, rowIdx * 128 + idx)

findEdgesRec :: (RowNodes, RowNodes) -> [Edge]
findEdgesRec (RowNodes cur, RowNodes next) = do
    let curTuples = zip cur (tail cur)
    let edgesWithinCur = concatMap (\(a1, a2) -> [(snd a1, snd a2) | fst a2 == 1 + fst a1]) curTuples
    let edgesAcrossRows = concatMap findCrossRowEdge cur
    edgesWithinCur ++ edgesAcrossRows
    where
        findCrossRowEdge x = case lookup (fst x) next of
            Just vert -> [(snd x, vert)]
            Nothing   -> []


findEdges :: [RowNodes] -> [Edge]
findEdges listOfRowNodes = concatMap findEdgesRec $ zip listOfRowNodes (tail listOfRowNodes)

bounds :: [Edge] -> (Vertex, Vertex)
bounds theEdges = let allV = map fst theEdges ++ map snd theEdges
                  in (minimum allV, maximum allV)

toGraph :: [RowNodes] -> Graph
toGraph listOfRowNodes = do
    let edges = findEdges listOfRowNodes
    buildG (bounds edges) edges

countRegions :: String -> Int
countRegions input = do
    let hashes = rowHashes input
    let listOfRowNodes = map rowHashToRowNodes hashes
    let g = toGraph listOfRowNodes
    length $ components g