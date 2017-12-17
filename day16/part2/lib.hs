{-# LANGUAGE BangPatterns #-}
module Lib where
import Data.Char
import Data.List.Split
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.STRef
import Data.Array.MArray
import Debug.Trace
import Data.Array.Base (unsafeRead, unsafeWrite, unsafeAt)

data DanceMove = Spin Int
               | Exchange Int Int
               | Partner Char Char deriving (Eq, Show)
--type Dancers = V.MVector s Char
--type Dancers = UArray Int Char

-- note: First is a single-elem array
data Dancers s = Dancers (STRef s Int) (STUArray s Int Char) (STUArray s Int Int) Int

toInt x = read x :: Int

firstTwo :: [a] -> (a, a)
firstTwo ss = (head ss, head $ tail ss) -- TODO: pattern matching ??

dancersToString :: Dancers s -> ST s String
dancersToString (Dancers i dancers indices _) = do
    idx <- readSTRef i
    el <- getElems dancers
    -- is <- getElems indices
    let (d,e) = splitAt idx el
    let str1 = e ++ d
    -- let charAt i = el!!i
    -- let (a,b) = splitAt idx is
    -- let orderedIndices = b ++ a
    -- let str2 = map intToChar (b ++ a)
    -- return $ check str1 str2
    -- let str3 = map charAt orderedIndices
    -- return str3
    return str1
    -- where 
    --     intToChar i = chr (i + 97)
    --     check s1 s2 = do
    --         if s1 /= s2 then
    --             error ("Mismatch: " ++ s1 ++ " != " ++ s2)
    --         else
    --             s2

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

createDancers :: Int -> ST s (Dancers s)
createDancers num = do
    indices <- thaw (listArray (0,num-1) [0..(num-1)] :: UArray Int Int)
    dancers <- thaw (listArray (0,num-1) $ map dancer [0..(num-1)] :: UArray Int Char)
    i <- newSTRef 0
    return $ Dancers i dancers indices num
    where dancer i = chr (97 + i)

swapInt :: STUArray s Int Int -> Int -> Int -> ST s ()
swapInt arr i1 i2 = do
    d1 <- readArray arr i1
    d2 <- readArray arr i2
    writeArray arr i1 d2
    writeArray arr i2 d1
    return ()
-- TODO: WTF? How do I write one function for this?? type X = Int | Char ??
swapChar :: STUArray s Int Char -> Int -> Int -> ST s ()
swapChar arr i1 i2 = do
    d1 <- readArray arr i1
    d2 <- readArray arr i2
    writeArray arr i1 d2
    writeArray arr i2 d1
    return ()
    

realIdx :: Dancers s -> Int -> ST s Int
realIdx (Dancers si s _ len) i2 = do
    startIdx <- readSTRef si
    return $ (startIdx + i2) `mod` len

virtIdx :: Dancers s -> Int -> ST s Int
virtIdx (Dancers si s _ len) i2 = do
    startIdx <- readSTRef si
    return $ (i2 - startIdx) `mod` len
                    
applyDanceMove :: Dancers s -> DanceMove -> ST s ()
applyDanceMove d@(Dancers si dancers indices arrLen) move = case move of
    Spin num       -> do
        let startIdx = arrLen - num
        newIdx <- realIdx d startIdx
        writeSTRef si newIdx
        return ()
    Exchange i1 i2 -> do
        -- calculate the real array indices (given the index offset)
        ri1 <- realIdx d i1
        ri2 <- realIdx d i2
        -- find che characters, so we can find the slots
        p1 <- readArray dancers ri1
        p2 <- readArray dancers ri2
        let slotOfP1 = charToInt p1
        let slotOfP2 = charToInt p2
        
        swapChar dancers ri1 ri2
        -- swapInt indices ri1 ri2
        swapInt indices slotOfP1 slotOfP2
        return ()
    Partner p1 p2  -> do
        -- calculate slot numbers - in each slot, we have the index of the corresponding dancer
        let slotOfP1 = charToInt p1
        let slotOfP2 = charToInt p2
        -- find the indices stored in the slots
        realIndexOfP1 <- readArray indices slotOfP1
        realIndexOfP2 <- readArray indices slotOfP2
        -- swap the dancers
        swapChar dancers realIndexOfP1 realIndexOfP2
        -- swap contents of the slots
        swapInt indices slotOfP1 slotOfP2

        -- figure out "virtual" indices (i.e. what )
        -- startIndex <- readSTRef si
        -- let virtIdx x = (x - startIndex) `mod` arrLen
        -- let v1 = virtIdx ix1
        -- let v2 = virtIdx ix2
        -- -- swap
        -- -- swapChar dancers ix1 ix2
        -- -- swapInt indices ix1 ix2
        -- applyDanceMove d (Exchange v1 v2)
        return ()
    where 
        charToInt c = ord c - 97

debugDancers :: Dancers s -> DanceMove -> ST s String
debugDancers (Dancers si dancers indices len) move = do
    idx <- readSTRef si
    ds <- getElems dancers
    is <- getElems indices
    let oks = map (isDancerOk ds is) [0..(len-1)]
    let nok = False `elem` oks
    let okstr = if nok then " NOK" else "    "
    return $ show idx ++ " " ++ show ds ++ " " ++ show is ++ okstr ++ " ==> " ++ show move
    where
        isDancerOk ds is i = do
            let expChar = chr (97 + i)
            let indexOfChar = is!!i
            let actChar = ds!!indexOfChar
            expChar == actChar


finalOrderMulti :: Int -> Int -> String -> String
finalOrderMulti repeatCount dancerCount str = do
    let moves = parseInput str -- a list
    runST $ do
        dancers <- createDancers dancerCount
        forM_ [1..repeatCount] $ \_ -> do
            forM_ moves $ \m -> do
                -- str <- debugDancers dancers m
                -- applyDanceMove (trace str dancers) m
                applyDanceMove dancers m
        dancersToString dancers
