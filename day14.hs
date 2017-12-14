import Data.Char -- For 'ord'
import KnottyHash
import Numeric
import Text.Printf (printf)

-- Get all sequences from a string
allHash :: String -> [String]
allHash str =
  let
    nums = take 127 [0,1..]
    strs = map (\x -> str ++ "-" ++ (show x)) nums
  in
    map knotHash strs

c2int c = (ord c) - (ord '0')

-- Convert hex string to integer
hexStrToInt :: String -> [Int]
hexStrToInt str = map hex' str
  where
    hex' c =
      if c <= '9'
      then c2int c
      else case c of
        'a' -> 10
        'b' -> 11
        'c' -> 12
        'd' -> 13
        'e' -> 14
        'f' -> 15
        
-- Int to binary (str)
i2b :: Int -> String
i2b x = printf "%b" x

-- Find the number of ones
numOnes :: String -> Int
numOnes [] = 0
numOnes (c:cs) =
  let
    val = if c == '1'
      then 1
      else 0
  in
    val + numOnes cs

numOnesList :: [String] -> Int
numOnesList lst = foldr (+) 0 (map numOnes lst)

hashesToLst :: [String] -> [Int]
hashesToLst = foldr1 (++) . (map (hexStrToInt . knotHash))


onesFromStr s = foldr1 (+) $ map numOnes (map i2b (hashesToLst (allHash s)))
