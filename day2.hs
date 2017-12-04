import System.IO  
import Control.Monad
import Data.Char
import Data.Ord
import Data.List

pairs :: [Int] -> [(Int, Int)]
pairs l = filter (\(x,y) -> x /= 1)[(x,y) | (x:ys) <- tails l, y <- ys]

maxList lst = foldr1 max lst
minList lst = foldr1 min lst
processRow :: [Int] -> Int
processRow lst = (maxList lst) - (minList lst)

part1 vals = sum (map processRow vals)

bestDiv vals = filter (\(x,y) -> (x `mod` y == 0) || (y `mod` x == 0)) (pairs vals)

divVal :: [(Int,Int)] -> Int
divVal vals
  | x > y = x `div` y
  | x <= y = y `div` x
  where
    (x,y) = head vals

part2 rows = sum (map divVal (map bestDiv rows))

main :: IO()
main = do
  vals <- replicateM 16 (fmap (map read . words) getLine :: IO [Int])
  print $ part1 vals
  print $ part2 vals

