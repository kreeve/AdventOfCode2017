import System.IO  
import Control.Monad
import Data.Char
import Data.List
import Test.QuickCheck

pairs :: [Int] -> [(Int, Int)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]
dirs = pairs [-1,0,1,-1,0,1]
adds (x,y) (z,w) = (x+z,y+w)
subs (x,y) (z,w) = (x-z,y-w)
swap (x,y) = (y,x)

-- Get the next (x,y) point
nextSpiral x y
  | y > 0 && x > -y  && x <= y = (x-1, y) -- top row
  | y < 0 && x <= (abs y) && x >= y = (x+1, y) -- bottom
  | x > 0 && y < x = (x, y+1) -- right side
  | x < 0 && y > x = (x, y-1) -- left side
  | otherwise = (0,0)
spiralNum :: Int -> (Int,Int)
spiralNum 1 = (0,0)
spiralNum 2 = (1,0)
spiralNum n =
  let (x,y) = spiralNum (n-1)
  in nextSpiral x y

-- Property for quickCheck to verify
oneToOneProp :: Int -> Int -> Bool
oneToOneProp m n
  | m <= 0 || n <= 0 = True
  | otherwise = if n /= m then spiralNum n /= spiralNum m else (spiralNum n == spiralNum m)

manhattan (x,y) = (abs x) + (abs y)

part1 val = manhattan (spiralNum val)

-- Find the spiral number at (x,y)    
inverseSpiral :: (Int,Int) -> Int
inverseSpiral (x,y) = head (filter (\n -> spiralNum n == (x,y)) [1,2..])

-- Find the neighbor squares
neighbors x y = [(x-1,y-1),(x-1,y),(x-1,y+1),
                 (x, y-1),(x,y),(x,y+1),
                 (x+1,y-1),(x+1,y),(x+1,y+1)]
                
-- Check if a neighbor to (x,y) is less that x,y
isDecreasing (x,y) (z,w) = (inverseSpiral (z,w)) < (inverseSpiral (x,y))

-- Find the nth accumulated value in the summing spiral
sumSpiral :: Int -> Int
sumSpiral 1 = 1
sumSpiral 2 = 1
sumSpiral n =
  let
    (x,y) = spiralNum n
  in
    sum (map (sumSpiral . inverseSpiral) (filter (\z -> isDecreasing (x,y) z) (neighbors x y)))

part2 val = head (map sumSpiral (filter (\x -> sumSpiral x > val) [1,2..]))

readInt :: IO Int
readInt = readLn

main :: IO()
main = do
  val <- readInt
  print $ part1 val
  print $ part2 val
