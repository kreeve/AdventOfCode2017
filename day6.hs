import System.IO  
import Control.Monad
import Data.Char
import Data.Ord
import Data.List
import Control.Monad.Fix

import Util

-- "Steal" 1 from first value into second, as long as first > 0
steal :: Int -> Int -> (Int,Int)
steal v1 v2
  | v1 > 0 = (v1-1,v2+1)
  | otherwise = (v1,v2)

-- Run one step of this function, distribute from val 
redistStep :: ([Int],Int,Int) -> ([Int], Int, Int)
redistStep (lst,idx,val) =
  let
    n = length lst
    v = lst !! idx
    (acc,v2) = steal val v
    modList = replace idx v2 lst
    nextIdx = if val > 0 then mod (idx+1) n else idx
  in
    (modList, nextIdx, acc)

-- Memory banks problem! Redistribute from largest to the rest, going in rightword order.
redist :: [Int] -> [Int]
redist lst =
  let
    (idx,m) = argMax lst
    i2 = (idx+1) `mod` (length lst)
    modList = replace idx 0 lst
    endVal = fixedPoint (redistStep) (modList,i2,m)
  in
    fst3 endVal

part1 :: [Int] -> Int
part1 lst = findCycle redist lst

part2 :: [Int] -> Int
part2 lst =
  let
    n = findCycle redist lst
    runNTimes n = foldr (.) id (replicate n redist)
    element = runNTimes n lst
    firstSeen = head (filter (\x -> (runNTimes x lst) == element) [0,1..])
  in
    n - firstSeen
main :: IO()
main = do
  val <- (fmap (map read . words) getLine :: IO [Int])
  print $ part1 val
  print $ part2 val
