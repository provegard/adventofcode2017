module Lib where
import Data.Foldable
import Data.List.Split
import Data.List
import Text.Read

data Triple = Triple { tx :: Int, ty :: Int, tz :: Int } deriving (Show, Eq)
data Particle = Particle { position :: Triple, acceleration :: Triple } deriving (Show, Eq)

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex = zip [0..]

parseInputLine :: String -> Particle
parseInputLine str = do
    let parts = splitOneOf ", <>" str
    let numbers = flatMap id (map tryParse parts)
    let threes = chunksOf 3 numbers
    let triples = map toTriple threes
    Particle (head triples) (triples!!2)
    where
        tryParse x = readMaybe x :: Maybe Int
        flatMap f = concatMap (toList . f)
        toTriple (a:b:c:_) = Triple a b c

manhattan :: Triple -> Int
manhattan (Triple x y z) = sum $ map abs [x, y, z]

indexOfClosest :: [Particle] -> Int
indexOfClosest particles = do
    let sorted = sortBy comp $ zipWithIndex particles
    (fst . head) sorted
    where
        comp (_, a) (_, b) = let x = compare (accel a) (accel b)
                             in if x == EQ then compare (pos a) (pos b) else x
        accel = manhattan . acceleration
        pos   = manhattan . position
    
longRun :: [String] -> Int -> Int
longRun strParticles upper = indexOfClosest $ map parseInputLine strParticles