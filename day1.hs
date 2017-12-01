import System.IO  
import Control.Monad
import Data.Char

-- Helper function for part 1
-- Add head of list to sum if it matches next element
sumFollowsHelper h (x:y:xs)
  | x == y = x + (sumFollowsHelper h ([y] ++ xs))
  | x /= y = sumFollowsHelper h ([y] ++ xs)
sumFollowsHelper h [] = 0
sumFollowsHelper h [x]
  | x == h = h
  | otherwise = 0
sumFollows (x:xs) = sumFollowsHelper x (x:xs)
sumFollows [] = 0

len (x:xs) = 1 + len xs
len [] = 0

-- Shift list by 1 in a circular manner
circle lst = (tail lst) ++ [head lst]

halfList lst = last (take ((len lst `div` 2) + 1) lst)

part2 n lst
  | notDone && isMatch = (head lst) + (part2 (n+1) (circle lst))
  | notDone = (part2 (n+1) (circle lst))
  | not notDone = 0
  where
    notDone = n < (len lst)
    isMatch = (head lst) == (halfList lst)

main :: IO()
main = do
  str <- getLine
  let
    lst = map digitToInt str
  print $ sumFollows lst
  print $ part2 0 lst
  
