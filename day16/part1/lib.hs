module Lib where
import Data.Char
import Data.List.Split
import qualified Data.Sequence as Seq

data DanceMove = Swap Int
               | Exchange Int Int
               | Partner String String deriving (Eq, Show)
type Dancers = Seq.Seq String

toInt x = read x :: Int

firstTwo :: [a] -> (a, a)
firstTwo ss = (head ss, head $ tail ss) -- TODO: pattern matching ??

parseInput :: String -> [DanceMove]
parseInput str = do
    let moves = splitOn "," str
    map toDanceMove moves
    where
        toDanceMove ('s':rest) = Swap $ toInt rest
        toDanceMove ('p':rest) = do
            let (p1,p2) = firstTwo $ splitOn "/" rest
            Partner p1 p2
        toDanceMove ('x':rest) = do
            let (s1,s2) = firstTwo $ splitOn "/" rest
            Exchange (toInt s1) (toInt s2)
        toDanceMove other = error ("Unknown dance move " ++ other)

createDancers :: Int -> Dancers
createDancers num = let dancer i = chr (97 + i) : ""
                     in Seq.fromList $ map dancer [0..(num-1)]

applyDanceMove :: Dancers -> DanceMove -> Dancers
applyDanceMove dancers move = case move of
    Swap num       -> let (a,b) = Seq.splitAt (Seq.length dancers - num) dancers
                      in (Seq.><) b a
    Exchange i1 i2 -> do
        let d1 = Seq.index dancers i1
        let d2 = Seq.index dancers i2
        let r1 = Seq.update i1 d2 dancers
        Seq.update i2 d1 r1
    Partner p1 p2  -> do
        let ix = Seq.findIndicesL (\d -> d == p1 || d == p2) dancers
        let (i1,i2) = firstTwo ix
        applyDanceMove dancers (Exchange i1 i2)

finalOrder :: Int -> String -> String
finalOrder dancerCount str = do
    let moves = parseInput str
    let dancers = createDancers dancerCount
    let positions = foldl applyDanceMove dancers moves
    concat positions