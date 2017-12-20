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
newtype ParticleSystem = ParticleSystem [Particle]

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

manhattan :: Particle -> Int
manhattan part = do
    let pos = position part
    sum $ map abs [tx pos, ty pos, tz pos]

addTrip :: Triple -> Triple -> Triple
addTrip t1 t2 = do
    let x' = tx t1 + tx t2
    let y' = ty t1 + ty t2
    let z' = tz t1 + tz t2
    Triple x' y' z'

tick :: Particle -> Particle
tick part = do
    let v' = addTrip (velocity part) (acceleration part)
    let p' = addTrip (position part) v' -- TODO: correct with v' rather than old velocity?
    Particle p' v' (acceleration part)

tickSystem :: ParticleSystem -> ParticleSystem
tickSystem (ParticleSystem particles) = ParticleSystem (map tick particles)

longRun' :: ParticleSystem -> Int -> Int
longRun' sys upper = do
    -- brute force
    let futureSystems = drop (upper - 1) (systems sys)
    let (ParticleSystem parts) = head futureSystems
    let indexed = zipWithIndex parts
    let sorted = sortOn mh indexed
    (fst . head) sorted
    where
        systems s = s : (systems . tickSystem) s
        mh (_,p) = manhattan p

longRun :: [String] -> Int -> Int
longRun strParticles upper = do
    let parts = map parseInputLine strParticles
    let sys = ParticleSystem parts
    longRun' sys upper