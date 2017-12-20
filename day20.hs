import Util
import Control.Monad
import System.Environment (getArgs)
import qualified Data.Set as Set

data Vec3 = Vec3 {x :: Int,
                  y :: Int,
                  z :: Int} deriving (Show, Eq, Ord) -- Always ints here

data Point = Point {pos :: Vec3,
                    vel :: Vec3,
                    acc :: Vec3} deriving (Show, Eq)

-- TODO : Use state monad or make own!
instance Num Vec3 where
  (Vec3 x y z) + (Vec3 x' y' z') = Vec3 (x+x') (y+y') (z+z')
  abs (Vec3 x y z) = Vec3 (abs x) (abs y) (abs z)

zeros :: Vec3
zeros = Vec3 {x=0,y=0,z=0}

origin :: Point
origin = Point {pos = zeros,
               vel = zeros,
               acc = zeros}

move :: Point -> Point
move pt =
  let
    vel' = (vel pt) + (acc pt)
    pos' = (pos pt) + vel'
  in
    Point pos' vel' (acc pt)

magnitude :: Vec3 -> Int
magnitude (Vec3 a b c) = manhattanDist [a,b,c] [0,0,0]

-- Farthest == highest accel
closest :: [Point] -> Int
closest pts = fst $ argMin $ (map (magnitude . acc)) pts

--Read in the V=<x,y,z> format
readVec :: String -> Vec3
readVec line =
  let
    [_,b] = split '<' line
    t = if (last b) == ','
      then (length b) - 2
      else (length b) - 1
    v = take t b
    [x,y,z] = map readInt (split ',' v)
  in
    Vec3 x y z

readPt :: String -> Point
readPt line =
  let
    clean = remChar 'p' $ remChar 'v' $ remChar 'a' $ remChar '=' $ line
    strs = words clean
    [p, v, a] = map readVec strs
  in
    Point p v a

-- Check for collisions
removeCollisions :: [Point] -> [Point]
removeCollisions pts =
  let
    dupes =
      rem' pts Set.empty Set.empty where
      rem' [] seen dupes = dupes
      rem' (p:ps) seen dupes =
        let
          dupes' = if Set.member (pos p) seen
            then Set.insert (pos p) dupes
            else dupes
          seen' = Set.insert (pos p) seen
        in
          rem' ps seen' dupes'
  in
    filter (\x -> not (Set.member (pos x) dupes)) pts

leftAfter :: [Point] -> Int -> Int
leftAfter points n = length $ last $ take n $ iterate (removeCollisions . (map move)) points
    
main :: IO()
main =
  do
    args <- getArgs
    content <- readFile (args !! 0)
    let
      linesOfFile = lines content
      points = map readPt linesOfFile
    print $ closest points
    print $ leftAfter points 100
