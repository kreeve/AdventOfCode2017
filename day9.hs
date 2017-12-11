import System.IO  
import Control.Monad
import Data.Char
import qualified Data.Set as Set
import Control.Monad.Fix
import Control.Applicative

import NanoParsec

-- Expressions
data Group =
  Garbage [Char]
  | BrkGroup [Group]
  | Identifier String deriving (Show, Eq)

eval :: Group -> Int -> Int
eval grp val = case grp of
  Garbage _ -> 0
  BrkGroup grps -> (val+1) + sum (map (\x -> eval x (val+1)) grps)
  _ -> val

groupList :: Parser [Group]
groupList = sepBy group spaces
  
group :: Parser Group
group = identifier <|> (garbage <|> brackGroup)

joiner :: String -> (Group -> Group -> [Group]) -> Parser (Group -> Group -> [Group])
joiner x f = reserved x >> return f

garbage :: Parser Group
garbage = angleBrackets garbageGroup

garbageGroup :: Parser Group
garbageGroup = do
  n <- anythingBut '>'
  return $ Garbage n

brackGroup :: Parser Group
brackGroup = braces brackInner

brackInner :: Parser Group
brackInner = do
  n <- groupList
  return $ BrkGroup n

plainIdentifier :: Parser Group
plainIdentifier = do
  m <- anyStr
  return (Identifier m)

identifier :: Parser Group
identifier = plainIdentifier

removeBang :: String -> String
removeBang [] = []
removeBang [c] = if c == '!'
  then []
  else [c]  
removeBang (c:j:cs) =
  if c == '!' then removeBang cs
  else c:(removeBang (j:cs))

run :: String -> Group
run = runParser group

score :: Group -> Int
score x = eval x 0

countGarbage :: Group -> Int
countGarbage (BrkGroup grp) = sum $ map countGarbage grp
countGarbage (Garbage cnt) = length cnt

main :: IO()
main = do
  line <- getLine
  let
    vals = removeBang line
  print $ (score . run) vals
  print $ (countGarbage . run) vals
