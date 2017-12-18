module KnottyHash where

import Text.Printf
import Data.List.Split
import Data.Bits
import System.IO
import Data.Char
import qualified Data.Vector.Storable as V
import qualified Data.Set as Set
import qualified Data.Text as T

import Test.QuickCheck

import Util

data ListState = ListState Int Int Int [Int] [Int] deriving (Show, Eq) -- Index, len, skip, lens, list

-- Slice a list from start to end
slice :: Int -> Int -> [Int] -> [Int]
slice start end lst =
  let
    trueEnd = if end > start then end else (end+(length lst))
    double = take ((length lst)*2) $ cycle lst
    front = take trueEnd double
  in
    drop start front

-- Rebuild a list, with the sublist starting at `start`
rebuild :: Int -> [Int] -> [Int] -> [Int]
rebuild start sub lst =
  let
    m = length sub
    n = length lst
    end = (mod (m + start) n)
    numInFront = if start < end
      then 0
      else (m + start) - n
    back = if numInFront == 0
      then drop end lst
      else take (m - numInFront) sub -- All if doesn't wrap
    mid = if end < start && end > 0
      then slice end start lst
      else take (m - numInFront) sub
    front = if numInFront > 0
      then drop (m - numInFront) sub
      else take start lst
  in
    take n $ front ++ mid ++ back
    

-- Reverse part of the list
revPart :: [Int] -> Int -> Int -> [Int]
revPart lst start len =
  let
    end = mod (start+len) ((length lst ))
    sub = reverse $ slice start end lst
  in
    rebuild start sub lst

-- Transform the state (one step)
update :: ListState -> ListState
update (ListState idx len skip lens lst) =
  let
    n = length lst
    lst' = revPart lst idx len
    idx' = mod (idx + len + skip) n
    len' = case lens of
      x:xs -> x
      [] -> len
    lens' = case lens of
      x:xs -> xs
      [] -> lens
    skip' = skip + 1
  in
    ListState idx' len' skip' lens' lst'

part1 :: ListState -> Int
part1 (ListState _ _ _ _ lst) = case lst of
  x:y:xs -> x*y
  _ -> 0

lstOf :: ListState -> [Int]
lstOf (ListState idx len skip lens lst) = lst

stateFrom :: [Int] -> Int -> ListState
stateFrom lens num = ListState 0 (head lens) 0 (tail lens) (take num [0,1..])

finalState :: ListState -> ListState
finalState state = case state of
  (ListState idx len skip lens lst) ->
    let
      n = length lens
      func = foldr (.) id (replicate (n+1) update)
    in
      func state

preservedProp lst =
  (length lst) > 0 && (length (filter (\x->x<0) lst)) == 0 ==>
  Set.fromList (lstOf (finalState (stateFrom lst 10))) == Set.fromList (lstOf (stateFrom lst 10))  

(//) :: Show a => Ord a => [a] -> [(Int, a)] -> [a]
(//) lst idxs = map (\i -> case lookup i idxs of
                        Just a -> a
                        Nothing -> lst !! i) (take (length lst) [0,1..])

vecOf :: ListState -> [Int]
vecOf (ListState _ _ _ _ v) = v

step :: ListState -> Int -> ListState
step (ListState p0 _ s0 _ v0) n = ListState p1 0 s1 [] v1
  where
    ixes = (\x -> mod (x+p0) (length v0)) <$> init [0 .. n]
    vals = (\x -> v0 !! x) <$> ixes
    v1   = v0 // zip ixes (reverse vals)
    p1   = mod (p0 + n + s0) (length v0)
    s1   = s0 + 1

process :: Int -> [Int] -> [Int]
process num lens = vecOf $ foldl step start lens
  where
    start = ListState 0 0 0 [] (take num [0,1..])
    
toHex :: [Int] -> [Char]
toHex = concatMap (printf "%02x" . foldr xor 0) . chunksOf 16

knotHash :: String -> String
knotHash = toHex . (process 256)
       . concat . replicate 64 . (++ salt)
       . map (fromIntegral . ord) . strip
  where
    salt  = [17, 31, 73, 47, 23]
    strip = T.unpack . T.strip . T.pack
    
