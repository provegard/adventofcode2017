{-# LANGUAGE BangPatterns #-}
module Lib where
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map
import Data.Char
import Data.Maybe
import Debug.Trace
import Data.Foldable

data Direction = South | North | East | West deriving (Eq, Show)
data Position = Position Int Int Direction deriving (Eq, Show)
type Row = Seq.Seq Char
type Rows = Seq.Seq Row
type Collected = String

startingPosition :: Rows -> Position
startingPosition rows = do
    -- scan the first row until we find '|'
    let row = Seq.index rows 0
    case Seq.findIndexL isStart row of
        Just colIdx -> Position 0 colIdx South
        Nothing     -> error "Failed to find starting position"
    where
        isStart x = x == '|'

nextPos :: Position -> Position
nextPos (Position r c dir) = case dir of
    South -> Position (r + 1) c dir
    North -> Position (r - 1) c dir
    East  -> Position r (c + 1) dir
    West  -> Position r (c - 1) dir

possibleTurns :: Direction -> (Direction, Direction)
possibleTurns dir = case dir of
    South -> (East, West)
    North -> (East, West)
    East  -> (South, North)
    West  -> (South, North)

isFinished :: Rows -> Position -> Bool
isFinished rows (Position r c _) = do
    if c < 0 || r < 0 || r >= Seq.length rows then True
    else do
        let row = Seq.index rows r
        if c >= Seq.length row then True
        else ' ' == Seq.index row c

turn :: Rows -> Position -> Position
turn rows pos@(Position r c dir) = do
    let (d1, d2) = possibleTurns dir
    if isDirectionOk d1 then nextPos (Position r c d1) 
    else if isDirectionOk d2 then nextPos (Position r c d2)
    else error ("no valid turn from " ++ show pos ++ ", candidates were " ++ show d1 ++ " and " ++ show d2)
    where
        isDirectionOk newDir = do
            let posToTest = nextPos (Position r c newDir)
            let invalid = isFinished rows posToTest
            not invalid
            -- not (trace ("posToTest = " ++ show posToTest ++ ", invalid = " ++ show invalid) invalid)

walk :: Rows -> Position -> Collected -> (Position, Collected)
walk rows position@(Position r c dir) collected = do
    if isFinished rows position then
        (position, collected)
    else do
        -- let newPos@(Position r c _) = nextPos position
        let row = Seq.index rows r
        let chr = Seq.index row c
        continue row chr
        where
            newPos = nextPos position
            continue row ch
                | ch == '+'       = walk rows (turn rows position) collected
                | isAsciiUpper ch = walk rows newPos (collected ++ [ch])
                | otherwise       = walk rows newPos collected

collectAll :: [String] -> String
collectAll lines = do
    let rows = Seq.fromList (map toRow lines)
    let pos = startingPosition rows
    let (p, c) = walk rows pos ""
    toList c
    where toRow = Seq.fromList
  