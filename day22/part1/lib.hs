{-# LANGUAGE BangPatterns #-}
module Lib where
import Data.Char
import Data.Maybe
import Debug.Trace
import Data.Foldable
import Data.List.Split
import Data.List
import Text.Read
import qualified Data.Map.Strict as Map

newtype Position = Position (Int, Int) deriving (Eq, Show, Ord)
data Status = Clean | Infected deriving (Eq, Show)
newtype Node = Node Status deriving (Eq, Show)
newtype Grid = Grid (Map.Map Position Node) deriving (Eq, Show)
data Direction = South | East | West | North deriving (Eq, Show)
data World = World Grid Position Direction Int deriving (Eq, Show)
data Turn = TLeft | TRight deriving (Eq, Show)

turn :: Direction -> Turn -> Direction
turn North TLeft  = West
turn North TRight = East
turn East TLeft   = North
turn East TRight  = South
turn South TLeft  = East
turn South TRight = West
turn West TLeft   = South
turn West TRight  = North

nodeAt :: Grid -> Position -> Maybe Node
nodeAt (Grid m) position = Map.lookup position m

newNode :: Node
newNode = Node Clean

treatNode :: Node -> Node
treatNode (Node Clean) = Node Infected
treatNode node         = Node Clean

whichTurn :: Node -> Turn
whichTurn (Node Infected) = TRight
whichTurn node            = TLeft

move :: Position -> Direction -> Position
move (Position (r,c)) North = Position (r-1,c)
move (Position (r,c)) South = Position (r+1,c)
move (Position (r,c)) East  = Position (r,c+1)
move (Position (r,c)) West  = Position (r,c-1)

updateGrid :: Grid -> Position -> Node -> Grid
updateGrid (Grid gm) pos node = Grid (Map.insert pos node gm)

burst :: World -> Int -> World
burst world 0 = world
burst (World grid position direction infected) left = do
    let node = fromMaybe newNode (nodeAt grid position)
    let newDir = turn direction (whichTurn node)
    let updatedNode@(Node status) = treatNode node
    let newPos = move position newDir
    let newInfected = if status == Infected then infected + 1 else infected
    let newGrid = updateGrid grid position updatedNode
    let w = World newGrid newPos newDir newInfected
    burst w (left - 1)

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex = zip [0..]
    
parseWorld :: [String] -> World
parseWorld lines = do
    let zippedLines = zipWithIndex lines
    let nodes = toNodes zippedLines
    let side = length lines
    let center = (side - 1) `div` 2
    World (Grid (Map.fromList nodes)) (Position (center,center)) North 0
    where
        toNodes :: [(Int, String)] -> [(Position, Node)]
        toNodes = concatMap (\(row,s) -> parseNodes s row 0)
        parseNodes :: String -> Int -> Int -> [(Position, Node)]
        parseNodes [] row col = []
        parseNodes (ch:rest) row col = do
            let status = if ch == '#' then Infected else Clean
            let pos = Position (row,col)
            let node = Node status
            (pos, node) : parseNodes rest row (col + 1)