{-# LANGUAGE BangPatterns #-}
module Lib where
import Data.Char
import Data.List.Split
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.STRef

data DanceMove = Swap Int
               | Exchange Int Int
               | Partner Char Char deriving (Eq, Show)
--type Dancers = V.MVector s Char
--type Dancers = UArray Int Char

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
            Partner (head p1) (head p2)
        toDanceMove ('x':rest) = do
            let (s1,s2) = firstTwo $ splitOn "/" rest
            Exchange (toInt s1) (toInt s2)
        toDanceMove other = error ("Unknown dance move " ++ other)

createDancers :: Int -> UArray Int Char
createDancers num = let dancer i = chr (97 + i)
                    in listArray (0,num-1) $ map dancer [0..(num-1)]

applyDanceMove :: STUArray s Int Char -> DanceMove -> ST s ()
applyDanceMove dancers move = case move of
    Swap num       -> do
        (_, upper) <- getBounds dancers
        let arrLen = upper + 1
        let startIdx = arrLen - num
        let newIdx i = (startIdx + i) `mod` arrLen
        temp <- mapIndices (0,upper) newIdx dancers -- can we avoid this allocation?
        forM_ [0..upper] $ \i -> do
            d <- readArray temp i
            writeArray dancers i d
        return ()
    Exchange i1 i2 -> do
        d1 <- readArray dancers i1
        d2 <- readArray dancers i2
        writeArray dancers i1 d2
        writeArray dancers i2 d1
        return ()
    Partner p1 p2  -> do
        -- let ix = Seq.findIndicesL (\d -> d == p1 || d == p2) dancers
        -- let (i1,i2) = firstTwo ix
        (_, upper) <- getBounds dancers
        i1 <- newSTRef (-1)
        i2 <- newSTRef (-1)
        forM_ [0..upper] $ \i -> do
            d <- readArray dancers i
            if d == p1 then
                writeSTRef i1 i
            else when (d == p2) $ writeSTRef i2 i
            -- TODO: break if both written
        ii1 <- readSTRef i1
        ii2 <- readSTRef i2
        applyDanceMove dancers (Exchange ii1 ii2)
        return ()

finalOrderMulti :: Int -> Int -> String -> String
finalOrderMulti repeatCount dancerCount str = do
    let moves = parseInput str -- a list
    let positionsArr = runSTUArray $ do
        dancers <- thaw $ createDancers dancerCount
        forM_ [1..repeatCount] $ \_ -> forM_ moves $ \m -> applyDanceMove dancers m
        return dancers
    elems positionsArr
