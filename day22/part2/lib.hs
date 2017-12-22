module Lib where
import Data.Maybe
import qualified Data.Map.Strict as Map

newtype Position = Position (Int, Int) deriving (Eq, Show, Ord)
data Status = Clean | Weakened | Infected | Flagged deriving (Eq, Show)
newtype Node = Node Status deriving (Eq, Show)
newtype Grid = Grid (Map.Map Position Node) deriving (Eq, Show)
data Direction = South | East | West | North deriving (Eq, Show)
data World = World Grid Position Direction Int deriving (Eq, Show)
data Turn = TLeft | TRight | TReverse deriving (Eq, Show) -- 'T' prefix to not collide with those in Prelude

turn :: Direction -> Turn -> Direction
turn North TLeft    = West
turn North TRight   = East
turn East TLeft     = North
turn East TRight    = South
turn South TLeft    = East
turn South TRight   = West
turn West TLeft     = South
turn West TRight    = North
turn North TReverse = South
turn South TReverse = North
turn East TReverse  = West
turn West TReverse  = East

nodeAt :: Grid -> Position -> Maybe Node
nodeAt (Grid m) position = Map.lookup position m

newNode :: Node
newNode = Node Clean

treatNode :: Node -> Node
treatNode (Node Clean)    = Node Weakened
treatNode (Node Weakened) = Node Infected
treatNode (Node Infected) = Node Flagged
treatNode (Node Flagged)  = Node Clean

whichTurn :: Node -> Maybe Turn
whichTurn (Node Clean)    = Just TLeft
whichTurn (Node Weakened) = Nothing
whichTurn (Node Infected) = Just TRight
whichTurn _               = Just TReverse

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
    let newDir = case whichTurn node of
            Just t  -> turn direction t
            Nothing -> direction -- same direction
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
        parseNodes [] _ _            = []
        parseNodes (ch:rest) row col = do
            let pos = Position (row,col)
            let node = Node (status ch)
            (pos, node) : parseNodes rest row (col + 1)
        status ch = if ch == '#' then Infected else Clean