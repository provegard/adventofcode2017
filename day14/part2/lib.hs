module Lib where
import Hash (realHash)
import Data.Graph
import Data.Maybe
import Data.Foldable
import Data.List

-- (row, col), 0-based
--newtype Coord = Coord (Int, Int)
type Node = Int
newtype Bits = Bits [Int]
data RowHash = RowHash Int Bits
newtype RowNodes = RowNodes [Node]
data ConnectedNode = ConnectedNode Node [Node] deriving (Eq, Show)

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

colIndexOf :: Node -> Int
colIndexOf node = node `mod` 128

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
        toNode (idx, _) = rowIdx * 128 + idx

findConnectedNodes :: RowNodes -> Maybe RowNodes -> [ConnectedNode]
findConnectedNodes (RowNodes nodes) maybeNextRow = do
    let acrossRows = case maybeNextRow of
            Just (RowNodes nextNodes) -> concatMap (findAcrossRow nextNodes) nodes
            Nothing -> []
    merged $ connectToNext nodes ++ acrossRows
    where
        connection a b = do
            let colA = colIndexOf a
            let colB = colIndexOf b
            if colA + 1 == colB then ConnectedNode a [b] else ConnectedNode a []
        connectToNext :: [Node] -> [ConnectedNode]
        connectToNext []          = []
        connectToNext [x]         = [ConnectedNode x []]
        connectToNext (x:y:rest)  = connection x y : connectToNext (y : rest)
        findAcrossRow nextNodes x = case find (\n -> colIndexOf n == colIndexOf x) nextNodes of
                Just other -> [ConnectedNode x [other]]
                Nothing    -> []
        merged listOfConnectedNodes = do
            let sorted = sortBy (\(ConnectedNode c1 _) (ConnectedNode c2 _) -> compare c1 c2) listOfConnectedNodes
            let grouped = groupBy (\(ConnectedNode c1 _) (ConnectedNode c2 _) -> c1 == c2) sorted
            map mergeNodes grouped
        mergeNodes connectedNodesWithSameNode = do
            let (ConnectedNode x _) = head connectedNodesWithSameNode
            let deps = concatMap (\(ConnectedNode _ d) -> d) connectedNodesWithSameNode
            ConnectedNode x deps

findAllConnectedNodes :: [RowNodes] -> [ConnectedNode]
findAllConnectedNodes = finder
    where
        finder []         = []
        finder [x]        = findConnectedNodes x Nothing
        finder (x:y:rest) = findConnectedNodes x (Just y) ++ finder (y : rest)

toGraph :: [RowNodes] -> Graph
toGraph listOfRowNodes = do
    let listOfConnectedNodes = findAllConnectedNodes listOfRowNodes
    let buildInput = map (\(ConnectedNode x deps) -> (x, x, deps)) listOfConnectedNodes
    let (g, _) = graphFromEdges' buildInput
    g

countRegions :: String -> Int
countRegions input = do
    let hashes = rowHashes input
    let listOfRowNodes = map rowHashToRowNodes hashes
    let g = toGraph listOfRowNodes
    length $ components g