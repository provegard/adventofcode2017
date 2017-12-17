module Lib where
import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Sequence as Seq
import Data.Foldable

data DanceMove = Spin Int
               | Exchange Int Int
               | Partner Char Char deriving (Eq, Show)
type Dancers = Seq.Seq Char

toInt x = read x :: Int

firstTwo :: [a] -> (a, a)
firstTwo ss = (head ss, head $ tail ss) -- TODO: pattern matching ??

dancersToString :: Dancers -> String
dancersToString = toList

parseInput :: String -> [DanceMove]
parseInput str = do
    let moves = splitOn "," str
    map toDanceMove moves
    where
        toDanceMove ('s':rest) = Spin $ toInt rest
        toDanceMove ('p':rest) = do
            let (p1,p2) = firstTwo $ splitOn "/" rest
            Partner (head p1) (head p2)
        toDanceMove ('x':rest) = do
            let (s1,s2) = firstTwo $ splitOn "/" rest
            Exchange (toInt s1) (toInt s2)
        toDanceMove other = error ("Unknown dance move " ++ other)

createDancers :: Int -> Dancers
createDancers num = let dancer i = chr (97 + i)
                    in Seq.fromList $ map dancer [0..(num-1)]

applyDanceMove :: Dancers -> DanceMove -> Dancers
applyDanceMove dancers move = case move of
    Spin num       -> let (a,b) = Seq.splitAt (Seq.length dancers - num) dancers
                      in (Seq.><) b a
    Exchange i1 i2 -> do
        let d1 = Seq.index dancers i1
        let d2 = Seq.index dancers i2
        let r1 = Seq.update i1 d2 dancers
        Seq.update i2 d1 r1 -- ugly with 2 updates, Vector's bulk update is nicer
    Partner p1 p2  -> do
        let ix = Seq.findIndicesL (\d -> d == p1 || d == p2) dancers
        let (i1,i2) = firstTwo ix
        applyDanceMove dancers (Exchange i1 i2)

finalOrderMulti :: Int -> Int -> String -> String
finalOrderMulti repeatCount dancerCount str = do
    let dancesUpToCycle = takeWhile notCycle (danceDanceDance repeatCount)
    -- add 1 because takeWhile won't include the dance positions when the cycle is found
    let cycleLength = 1 + length dancesUpToCycle
    let actualRepeatCount = repeatCount `mod` cycleLength
    -- grab the sought dance result from the done dances, since actualRepeatCount < cycleLength
    let newDancers = if actualRepeatCount == 0 then initialDancers else dancesUpToCycle!!(actualRepeatCount-1)
    dancersToString newDancers
    where
        moves = parseInput str
        initialDancers = createDancers dancerCount
        notCycle ds = dancersToString ds /= dancersToString initialDancers
        oneDance ds = foldl applyDanceMove ds moves
        danceDanceDance' ds 0 = []
        danceDanceDance' ds n = let next = oneDance ds
                                in next : danceDanceDance' next (n-1)
        danceDanceDance = danceDanceDance' initialDancers