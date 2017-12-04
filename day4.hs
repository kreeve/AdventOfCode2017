import System.IO  
import Control.Monad
import Data.Char
import Data.Ord
import Data.List

-- Find out if this has any repeats
validPassPhrase :: (Eq a) => [a] -> Bool
validPassPhrase lst = (length (nub lst)) == (length lst)

--Count valid ones
part1 :: Eq a => [[a]] -> Int
part1 lst = length (filter validPassPhrase lst)

--For part 2, sort each word to normalize
part2 :: Eq a => Ord a => [[[a]]] -> Int
part2 lst = length (filter (\row -> validPassPhrase (map sort row)) lst)

main :: IO()
main = do
  lines <- replicateM 512 getLine
  let
    vals = map words lines
  print $ part1 vals
  print $ part2 vals
