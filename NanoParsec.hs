module NanoParsec where

import Data.Char
import Control.Monad
import Control.Applicative

newtype Parser a = Parser {parse :: String -> [(a,String)]}

-- Run a parser, get your AST!
runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res,[])] -> res
    [(_,rs)] -> error $ "Stream not consumed"
    _ -> error $ "Unknown error"

item :: Parser Char
item = Parser $ \s ->
  case s of
    [] -> []
    (c:cs) -> [(c,cs)]

-- Bind from one parser type to another (AST type?)
bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> concatMap (\(a,s') -> parse (f a) s') $ parse p s

-- Return a single parser
unit :: a -> Parser a
unit a = Parser (\s -> [(a,s)])

instance Functor Parser where
  fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a,b) <- cs s])

instance Applicative Parser where
  pure = return
  (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

instance Monad Parser where
  return = unit
  (>>=) = bind

instance MonadPlus Parser where
  mzero = failure
  mplus = combine

instance Alternative Parser where
  empty = mzero
  (<|>) = option

combine :: Parser a -> Parser a -> Parser a
combine p q = Parser (\s -> parse p s ++ parse q s)

failure :: Parser a
failure = Parser (\cs -> [])

-- Choose between two parsers
option :: Parser a -> Parser a -> Parser a
option p q = Parser $ \s ->
  case parse p s of
    [] -> parse q s
    res -> res
    
-- Parse if character satisfies some condition
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item `bind` \c ->
  if p c
  then unit c
  else (Parser (\cs -> []))

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (flip elem s)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do { a <- p; rest a}
  where rest a = (do f <- op
                     b <- p
                     rest (f a b))
                 <|> return a

char :: Char -> Parser Char
char c = satisfy (c==)

string :: String -> Parser String
string [] = return []
string (c:cs) = do {char c; string cs; return (c:cs)}

spaces :: Parser String
spaces = many $ oneOf ", "

token :: Parser a -> Parser a
token p = do { a <- p; spaces ; return a}

reserved :: String -> Parser String
reserved s = token (string s)

braces :: Parser a -> Parser a
braces m =
  do
    reserved "{"
    n <- m
    reserved "}"
    return n

angleBrackets :: Parser a -> Parser a
angleBrackets m =
  do
    reserved "<"
    n <- m
    reserved ">"
    return n

sepBy :: Parser a1 -> Parser a -> Parser [a1]
sepBy p sep = sepBy1 p sep <|> return []

sepBy1 :: Parser a1 -> Parser a -> Parser [a1]    
sepBy1 p sep =
  do
    x <- p
    xs <- many (sep >> p)
    return (x:xs)
    
canceledItem :: Parser a -> Parser a
canceledItem m =
  do
    reserved "!"
    n <- m
    return n
  
anyStr :: Parser String
anyStr = some $ satisfy isAlpha

anythingBut :: Char -> Parser String
anythingBut a = many $ satisfy (\x -> x /= a)
