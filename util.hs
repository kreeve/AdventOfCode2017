module Util(replace
           , argMax
           , argMin
           , fixedPoint
           , fst3
           , findCycle
           , remChar
           , allSame
           , majority
           , split
           , readInt
           , manhattanDist) where
import Data.Maybe
import qualified Data.Set as Set
-- Take an index to a sequence, replace that index w/ modified value
replace :: Int -> Int -> [Int] -> [Int]
replace idx val seq = (take idx seq) ++ [val] ++ (drop (idx+1) seq)


-- Find index with largest value.
argMax :: Ord a => [a] -> (Int,a)
argMax = argThing max

argMin :: Ord a => [a] -> (Int,a)
argMin = argThing min

argThing :: Ord a => (a -> a -> a) -> [a] -> (Int,a)
argThing f lst =
  let
    n = length lst
    m = foldr1 f lst
    zipped = zip (take (length lst) [0,1..]) lst
  in
    let
      bestTuple = head (Prelude.filter (\x -> (snd x)==m) zipped)
    in
      bestTuple

-- Find a fixed point of a function, starting at `val`
fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint func start
  | start == val = val
  | otherwise = fixedPoint func val
  where
    val = func start

-- Grab first element of a 3tuple
fst3 :: (a,b,c) -> a
fst3 (x,y,z) = x

-- Helper function
findCycleHelper :: Eq a => (a->a) -> (a,a) -> Int -> Maybe (a,Int)
findCycleHelper func (tort,hare) num
  | t2 == h2 = Just (t2,num)
  | t2 == tort || h2 == hare = Nothing -- Two separate fixed points
  | otherwise = findCycleHelper func (t2,h2) (num + 1)
  where
    t2 = func tort
    h2 = func (func hare)

-- Set version of helper
findCycleSetHelper :: (Eq a,Ord a) => (a->a) -> a -> Set.Set a -> Int -> Int
findCycleSetHelper func val seen num
  | Set.member val seen = num
  | otherwise = findCycleSetHelper func v2 s2 num+1
  where
    v2 = func val
    s2 = Set.union seen (Set.fromList [val])

-- Find a cycle in a function. Sort of like in a list, but `next` is function application.
-- Using `tortoise and hare` algorithm
findCycle :: (Eq a, Ord a) => (a->a) -> a -> Int
findCycle func start =
  let
    res = findCycleSetHelper func start (Set.fromList []) 0
  in
    res

-- Remove a char from a str (any list actually)
remChar :: Eq a => a -> [a] -> [a]
remChar ch str = Prelude.filter (\x -> x /= ch) str

-- All elements same in list?
allSame :: Eq a => [a] -> Bool
allSame [] = True
allSame [_] = True
allSame (x:y:xs) = (x == y) && allSame (y:xs)

-- Majority of a list
majority :: Eq a => [a] -> a
majority lst =
  let
    count e = length (filter (\x -> x == e) lst)
    counts = map (\e -> count e) lst
    (idx,_) = argMax counts
  in
    lst !! idx

-- Parse commas
split :: Char -> String -> [String]
split _ "" = []
split c s = firstWord : (split c rest)
    where firstWord = takeWhile (/=c) s
          rest = drop (length firstWord + 1) s

readInt :: String -> Int
readInt = read

manhattanDist :: Num a => [a] -> [a] -> a
manhattanDist (x:xs) (y:ys) = abs (x-y) + manhattanDist xs ys
manhattanDist [] [] = 0
manhattanDist l1 l2 = error $ "Mismatched lenghts."
