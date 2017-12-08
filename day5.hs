import System.IO  
import Control.Monad
import Data.Char
import Data.Ord
import Data.List

-- Take an index to a sequence, replace that index w/ modified value
replace :: Int -> Int -> [Int] -> [Int]
replace idx val seq = (take idx seq) ++ [val] ++ (drop (idx+1) seq)

-- Take in an index to a sequence, return number of moves until out
jmp :: Int -> [Int] -> (Int -> Int) -> Int
jmp idx seq incr
  | idx >= n || idx < 0 = 0
  | otherwise =
      let
        modSeq = replace idx (incr val) seq
        next = idx + val
      in
        1 + (jmp next modSeq incr)
  where
    val = seq !! idx
    n = length seq

-- Part 1
part1 :: Int -> [Int] -> Int
part1 idx seq = jmp idx seq (\x -> x + 1)

-- Custom increment logic for part 2
customIncr :: Int -> Int
customIncr val
  | val >= 3 = val - 1
  | otherwise = val + 1

-- Part 2
part2 :: Int -> [Int] -> Int
part2 idx seq = jmp idx seq customIncr

main :: IO()
main = do
  lines <- replicateM 1097 getLine
  let
    seq = map (read :: String -> Int) lines
  print $ part1 0 seq
  print $ part2 0 seq
