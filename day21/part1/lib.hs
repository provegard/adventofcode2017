{-# LANGUAGE BangPatterns #-}
module Lib where
import Data.Char
import Data.Maybe
import Debug.Trace
import Data.Foldable
import Data.List.Split
import Data.List
import Text.Read


newtype Grid = Grid [String] deriving (Show, Eq)
data Rule = Rule Grid Grid deriving (Show, Eq)

parseGrid :: String -> Grid
parseGrid str = Grid $ splitOn "/" str

parseRule :: String -> Rule
parseRule str = do
    let (x:y:z:_) = splitOn " " str
    Rule (parseGrid x) (parseGrid z)

flipGrid :: Grid -> Grid
flipGrid (Grid lines) = Grid $ map reverse lines

flipGridV :: Grid -> Grid
flipGridV (Grid lines) = Grid $ reverse lines

rotateGrid :: Grid -> Grid
rotateGrid (Grid lines) = Grid $ rot lines
    where
        rot ls = do
            let first = reverse $ heads ls
            let cont = rest ls
            if null (head cont) then [first] else first : rot cont
        heads = map head
        rest = map (drop 1)

matches :: Grid -> Grid -> Bool
matches expected actual | expected == actual = True
matches expected actual | expected == flipGrid actual = True
matches expected actual | expected == flipGridV actual = True
matches expected actual = do
    let rotations = drop 1 $ scanl (\g _ -> rotateGrid g) actual [0..2]
    any matchRot rotations
    where
        matchRot r | expected == r = True
        matchRot r | expected == flipGrid r = True
        matchRot r | expected == flipGridV r = True
        matchRot r = False

gridSize :: Grid -> Int
gridSize (Grid lines) = length lines

splitGrid :: Grid -> [Grid]
splitGrid grid@(Grid lines) = do
    let chunkSize = if gridSize grid `mod` 2 == 0 then 2 else 3
    -- for a grid of size 4, chunkedLines will be a list of length 2, each item containing 2 lines
    -- i.e. the grid split vertically
    let chunkedLines = chunksOf chunkSize lines
    let linesOfGrids = map (`splitHor` chunkSize) chunkedLines
    concat linesOfGrids
    where 
        splitHor :: [String] -> Int -> [Grid]
        splitHor cl size = do
            -- split each incoming line into chunks
            let g = Grid $ heads size cl
            let cont = rest size cl
            if null (head cont) then [g] else g : splitHor cont size
        heads n = map (take n)
        rest n = map (drop n)
            
startGrid :: Grid
startGrid = Grid [".#.", "..#", "###"]

pixelsOn :: Grid -> Int
pixelsOn (Grid lines) = do
    let perLine = map onForLine lines
    sum perLine
    where
        onForLine line = length $ filter ('#' ==) line

assembleGrids :: [Grid] -> Grid
assembleGrids grids = do
    let side = isqrt $ length grids
    let linesOfGrids = chunksOf side grids
    Grid $ concatMap gridLines linesOfGrids
    where
        isqrt x = floor . sqrt $ (fromIntegral x :: Float)
        gridLines :: [Grid] -> [String]
        gridLines grids = do
            -- transform [Grid] into [[String]]
            let justLines = map (\(Grid ls) -> ls) grids
            gridLines' justLines
        gridLines' :: [[String]] -> [String]
        gridLines' ls = do
            -- concat the first from each
            let firstLine = concat $ heads ls
            let cont = rest ls
            if null (head cont) then [firstLine] else firstLine : gridLines' cont
        heads = map head
        rest = map (drop 1)


-- transforms a 2x2 or 3x3 grid
-- find a rule where the left-hand side matches the grid
transform' :: Grid -> [Rule] -> Grid
transform' grid rules = case find isMatchingRule rules of
    Just (Rule _ right) -> right
    Nothing -> error ("no matching rule for " ++ show grid)
    where
        isMatchingRule (Rule left _) = matches grid left

transform :: Grid -> [Rule] -> Grid
transform grid rules = do
    let smallerGrids = splitGrid grid
    let transformed = map (`transform'` rules) smallerGrids
    let res = assembleGrids transformed
    trace ("-> " ++ show res) res

run :: [String] -> Int -> Int
run input iterations = do
    let rules = map parseRule input
    let grid = startGrid
    let result = foldl (\g _ -> transform g rules) grid [1..iterations]
    pixelsOn result