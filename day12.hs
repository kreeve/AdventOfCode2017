import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad
import Util
type Graph a = Map.Map a [a]

-- Number of nodes reachable
numReachable :: Ord a => Graph a -> a -> Int
numReachable g start = length $ reachingSet g start

-- Set of nodes reachable (note to self, combine w/ above!!)
reachingSet :: Ord a => Graph a -> a -> Set.Set a
reachingSet g start = reach' g start Set.empty Set.empty
  where
    reach' :: Ord a1 => Graph a1 -> a1 -> Set.Set a1 -> Set.Set a1 -> Set.Set a1
    reach' g start seen frontier =
      let
        seen' = Set.insert start seen
        children = case Map.lookup start g of
          Nothing -> []
          Just lst -> lst
        validChildren = Set.fromList (filter (\x -> Set.notMember x seen') children)
        frontier' = Set.union frontier validChildren
        (nextNode, f) = Set.deleteFindMin frontier' --Pop from queue
      in
        if Set.null frontier'
        then seen'
        else (reach' g nextNode seen' f)

updateGraph :: Ord a => a -> [a] -> Graph a -> Graph a
updateGraph n childs g =
  let g' = Map.insert n childs g
  in
    update' n childs g'
    where
      update' name children graph =
        case children of
          (c:cs) ->
            let
              c' = case Map.lookup c graph of
                Just subs -> subs
                Nothing -> []
              c'' = Set.toList (Set.fromList (name:c'))
              g'' = Map.insert c c'' graph
            in
              update' n cs g''
          [] -> graph
    

parseLine :: String -> Graph String -> Graph String
parseLine line g =
  let
    [name,_,blob] = words line
    children = split ',' blob
  in
    updateGraph name children g

readGraph :: [String] -> Graph String
readGraph lines = readGraph' lines Map.empty
  where
    readGraph' [] g = g
    readGraph' (x:xs) g =
      let
        g' = parseLine x g
      in
        readGraph' xs g'

strip :: String -> String
strip [] = []
strip [x] = [x]
strip (c:d:cs) =
  if (c == ',' && d == ' ')
  then c:(strip cs)
  else c:(strip (d:cs))

allNodes :: Graph a -> [a]
allNodes g = Map.keys g

-- Find number of islands
islands :: Ord a => Graph a -> Int
islands graph =
  let
    nodes = allNodes graph
  in
    island' nodes graph Set.empty
    where
      island' :: Ord a1 => [a1] -> Graph a1 -> Set.Set a1 -> Int
      island' [] _ _ = 0
      island' (n:ns) graph visited =
        let
          nvisit = reachingSet graph n
          visited' = Set.union nvisit visited
          islandHere =
            if Set.null (nvisit Set.\\ visited)
            then 0
            else 1
        in
          islandHere + island' ns graph visited'
          
main :: IO()
main =
  do
    lines <- replicateM 7 getLine
    let
      graph = readGraph $ map strip lines
      firstNode = head (head lines)
    
    print $ numReachable graph [firstNode]
    print $ islands graph
