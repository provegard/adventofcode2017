{-# LANGUAGE BangPatterns #-}
module Lib where
import Data.Char
import Data.Maybe
import Debug.Trace
import Data.Foldable
import Data.List.Split
import Data.List
import Text.Read


data Triple = Triple { tx :: Int, ty :: Int, tz :: Int } deriving (Show, Eq)
data Particle = Particle { position :: Triple, velocity :: Triple, acceleration :: Triple } deriving (Show, Eq)
data IndexedParticle = IndexedParticle Int Particle deriving (Show, Eq)
newtype ParticleSystem = ParticleSystem [IndexedParticle]

-- toInt x = fromMaybe (error ("Not a number: " ++ show x)) (readMaybe x :: Maybe Int)
    
zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex = zip [0..]

parseInputLine :: String -> Particle
parseInputLine str = do
    let parts = splitOneOf ", <>" str
    let numbers = flatMap id (map tryParse parts)
    let threes = chunksOf 3 numbers
    let triples = map toTriple threes
    Particle (head triples) (triples!!1) (triples!!2)
    where
        tryParse x = readMaybe x :: Maybe Int
        flatMap f = concatMap (toList . f)
        toTriple (a:b:c:_) = Triple a b c

-- manhattan :: Particle -> Int
-- manhattan part = do
--     let pos = position part
--     sum $ map abs [tx pos, ty pos, tz pos]

addTrip :: Triple -> Triple -> Triple
addTrip t1 t2 = do
    let x' = tx t1 + tx t2
    let y' = ty t1 + ty t2
    let z' = tz t1 + tz t2
    Triple x' y' z'

hasCollided :: IndexedParticle -> IndexedParticle -> Bool
hasCollided (IndexedParticle _ p1) (IndexedParticle _ p2) = do
    let pos1 = position p1
    let pos2 = position p2
    pos1 == pos2

tick :: IndexedParticle -> IndexedParticle
tick (IndexedParticle i part) = do
    let v' = addTrip (velocity part) (acceleration part)
    let p' = addTrip (position part) v' -- TODO: correct with v' rather than old velocity?
    IndexedParticle i $ Particle p' v' (acceleration part)

pairs :: [IndexedParticle] -> [(IndexedParticle,IndexedParticle)]
pairs parts = [(x,y) | (x:ys) <- tails parts, y <- ys]

-- collisionsRemoved :: [IndexedParticle] -> [IndexedParticle]
-- collisionsRemoved parts = do
--     let tups = filter (\(p1,p2) -> not (hasCollided p1 p2)) (pairs parts)
--     -- ugh, inefficient
--     let listAgain = concatMap (\(p1,p2) -> p1:[p2]) tups
--     nub listAgain

collisionsRemoved :: [IndexedParticle] -> [IndexedParticle]
collisionsRemoved [] = []
collisionsRemoved [x] = [x]
collisionsRemoved (x:rest) = do
    let newList = filter (not . hasCollided x) rest
    if length newList == length rest then
        -- no collisions, so keep x
        x : collisionsRemoved rest
    else
        collisionsRemoved newList        

tickSystem :: ParticleSystem -> ParticleSystem
tickSystem (ParticleSystem particles) = do
    let newParts = collisionsRemoved $ map tick particles
    ParticleSystem newParts
    
leftStanding' :: ParticleSystem -> Int -> Int -> ParticleSystem
leftStanding' sys@(ParticleSystem parts) stabilityPeriod stable
    | stable == stabilityPeriod = sys
    | otherwise = do
        let newSys@(ParticleSystem newParts) = tickSystem sys
        if length parts == length newParts then
            -- nothing removed
            leftStanding' newSys stabilityPeriod (stable + 1)
        else
            leftStanding' newSys stabilityPeriod 0

leftStanding :: [String] -> Int -> [Int]
leftStanding strParticles stabilityPeriod = do
    let parts = zipWithIndex $ map parseInputLine strParticles
    let indexed = map (uncurry IndexedParticle) parts
    let sys = ParticleSystem indexed
    let (ParticleSystem parts) = leftStanding' sys stabilityPeriod 0
    let indices = map (\(IndexedParticle i _) -> i) parts
    indices