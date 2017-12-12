import qualified Data.Set as Set
import qualified Data.Map as Map
import Util
import Heap

type Pos = (Int, Int)
type Dir = (Int, Int)

north :: Dir
north = (0,-1)

south :: Dir
south = (0, 1)

east :: Dir
east = (1,0)

west :: Dir
west = (-1,0)

northeast :: Dir
northeast = (1,-1)

southeast :: Dir
southeast = (1,1)

northwest :: Dir
northwest = (-1,-1)

southwest :: Dir
southwest = (-1,1)


move :: Pos -> Dir -> Pos
move (x,y) (z,w) = (x+z, y+w)

allDirs :: [Dir]
allDirs = [north, south, east, west, northeast, southeast, northwest, southwest]

neighbors :: Pos -> [Pos]
neighbors pos = map (\d -> move pos d) allDirs

smallestVertex :: [(Int,Pos)] -> (Int,Pos)
smallestVertex q =
  let
    q' = reverse q
    minVal = foldr1 min (map snd q')
  in
    head (filter (\x -> (snd x) == minVal) q')

inList :: Eq a => [a] -> a -> Bool
inList [] _ = True
inList (x:xs) y = (x /= y) && (inList xs y)

manhattanDist :: Pos -> Pos -> Int
manhattanDist (x,y) (z,w) =
  (abs (x-z)) + (abs (y-w))

-- Shortest path from point a to point b
shortestPath :: (Int,Pos) -> Pos -> Set.Set (Int,Pos) -> (Set.Set Pos) -> Map.Map Pos Int -> Int
shortestPath (cost,cur) dest queue seen memo
  | cur == dest = cost
  | otherwise =
    case Map.lookup cur memo of
      Just d -> d
      Nothing ->
        let
          seen' = seen `Set.union` (Set.fromList [cur])
          nbs = neighbors cur -- Our neighbors
          neighborDist = cost + 1
          toAdd = filter (\x -> not (Set.member x seen)) nbs
          nq = Set.union (Set.fromList (map (\x -> (neighborDist,x)) toAdd)) queue
          next = Set.findMin nq
          d = manhattanDist cur dest
          nq' = Set.filter (\x -> x /= next && ((manhattanDist (snd x) dest) < d)) nq
        in
          shortestPath next dest nq' seen' memo
      
readDir :: String -> Dir
readDir s = case s of
  "n" -> north
  "s" -> south
  "e" -> east
  "w" -> west
  "ne" -> northeast
  "nw" -> northwest
  "se" -> southeast
  "sw" -> southwest

-- Memoized calculation
manyDist :: [Pos] -> Map.Map Pos Int -> [Int]
manyDist [] _ = []
manyDist (x:xs) memo =
  let
    dist = shortestPath (0,(0,0)) x Set.empty Set.empty memo
    memo' = Map.insert x dist memo
  in
    dist:(manyDist xs memo')
  
main :: IO()
main =
  do
    line <- getLine
    let
      dirs = map readDir (split ',' line)
      finalPos = foldr move (0,0) dirs
      allPos = scanl move (0,0) dirs
      allDist = map (\p -> shortestPath (0,p) (0,0) Set.empty Set.empty Map.empty) allPos 
    print $ finalPos
    print $ shortestPath (0,(0,0)) finalPos Set.empty Set.empty Map.empty
    print $ foldr1 max allDist
