import System.IO  
import Control.Monad
import Data.Char
import Data.List

pairs :: [Int] -> [(Int, Int)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]
dirs = pairs [-1,0,1,-1,0,1]
adds (x,y) (z,w) = (x+z,y+w)
subs (x,y) (z,w) = (x-z,y-w)
swap (x,y) = (y,x)

spiralNum :: Int -> (Int,Int)
spiralNum 1 = (0,0)
spiralNum 2 = (1,0)
spiralNum n
  | x >= 0 && x == -y = (x+1,y)
  | x == y && x < 0 = (x+1,y)
  | x == y && x > 0 = (x-1,y)
  | x >= 0 && y < 0 = (x+1, y)
  | x > y = (x,y+1)
  | x < 0 && y > x = (x,y-1) --have to go down here
  | x >= 0 && x < y = (x-1,y)
  | x >= 0 && x > y && y < 0 = (x+1,y)
  where
    (x,y) = spiralNum (n-1)


