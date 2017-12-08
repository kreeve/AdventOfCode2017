import System.IO  
import Control.Monad
import Data.Char
import Data.Ord
import Data.List
import qualified Data.Set as Set
import Control.Monad.Fix

import Util

data Elem = Prog String Int deriving (Eq, Show)
data Tree a = Empty | Node a [(Tree a)] deriving (Eq, Show)

-- Find the root of a graph (ordered edge pairs)
findRoot :: Eq a => [(a,a)] -> a
findRoot graph =
  let
    endPoints = map (\x -> snd x) graph
    allPoints = map (\x -> fst x) graph
  in
    head (filter (\x -> not (x `elem` endPoints)) allPoints)

-- Parse a line of this thing into a list of (start,end) edge tuples
parse :: [String] -> [(String,String)]
parse line
  | n <= 2 = []
  | otherwise =
    let
      root = line !! 0
      weight = line !! 1 -- not used YET
      endPoints = map (remChar ',') (drop 3 line)
    in
      map (\p -> (root,p)) endPoints
  where
    n = length line

-- Create the graph from a list of lines
createGraph :: [[String]] -> [(String, String)]
createGraph [] = []
createGraph (x:xs) = (parse x) ++ createGraph xs

-- Complete part 1!
part1 :: [[String]] -> String
part1 lines =
  let
    graph = createGraph lines
  in
    findRoot graph

-- Read a line of input, grab the weight
getWeight :: [String] -> (String,Int)
getWeight line =
  let
    raw = remChar '(' (remChar ')' (line !! 1))
    name = line !! 0
  in
    (name, (read :: String -> Int) raw)
    
-- Lookup and discard maybe
safeLook :: Eq a => a -> b -> [(a,b)] -> b
safeLook x dft tbl =
  let
    resOpt = lookup x tbl
  in
    case resOpt of
      Nothing -> dft
      Just x -> x
      
-- Get all endpts
allEdges :: Eq a => a -> [(a,a)] -> [a]
allEdges e graph =
  map snd (filter (\x -> (fst x) == e) graph)

nameOf :: Elem -> String
nameOf (Prog a _) = a

-- Read the tuple form into the other form.
readGraphForm :: [[String]] -> Tree Elem
readGraphForm lines =
  let
    tupleForm = createGraph lines
    allNames = Set.toList (Set.fromList ((map fst tupleForm) ++ (map snd tupleForm)))
    weights = map getWeight lines
    elemTable = map (\name -> (name, Prog name (safeLook name 0 weights))) allNames
    getProg name = safeLook name (Prog "foo" 0) elemTable
    elems = map snd elemTable
    makeTree name = Node (getProg name) (map makeTree (allEdges name tupleForm))
  in
    makeTree (findRoot tupleForm)

-- Get weight of a tree
weightOf :: (Tree Elem) -> Int
weightOf (Node (Prog _ w) []) = w
weightOf (Node (Prog _ w) childs) = (sum (map weightOf childs)) + w

-- Name of tree node
treeName :: (Tree Elem) -> String
treeName (Node (Prog n _) _) = n

-- Is the tree rooted at `node` balanced?
isBalanced :: (Tree Elem) -> Bool
isBalanced (Node _ childs) =
  let
    childWeights = map weightOf childs
  in
    allSame childWeights

-- How much until we're balanced?
adjustForBalance :: (Tree Elem) -> Int -> Int
adjustForBalance root toBal
  | isBalanced root =
    case root of
      (Node (Prog n w) childs) ->
        let
          weightDiff = toBal - (weightOf root)
        in
          if weightDiff == 0
          then 0
          else w + weightDiff
  | otherwise =
    case root of
      (Node (Prog _ w) childs) ->
        let
          weights = map weightOf childs
          maj = majority weights
          nonMaj = head (filter (\c -> (weightOf c) /= maj) childs)
        in
          adjustForBalance nonMaj maj

-- This one took a lot of debuging, hence the extra func
debugTree :: (Tree Elem) -> Int -> (String,Int)
debugTree root toBal
  | isBalanced root =
    case root of
      (Node (Prog n w) childs) ->
        ("end",w)
  | otherwise =
    case root of
      (Node (Prog _ w) childs) ->
        let
          weights = map weightOf childs
          maj = majority weights
          nonMaj = head (filter (\c -> (weightOf c) /= maj) childs)
        in
          case nonMaj of
            (Node (Prog name ww) cc) -> (name, ww)

        
main :: IO()
main =
  do
    lines <- replicateM 1193 getLine
    let
      vals = map words lines
    print $ part1 vals
    print $ adjustForBalance (readGraphForm vals) 0

