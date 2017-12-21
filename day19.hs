-- Strategy: read into vector, move based on vector
import Data.Char
import System.Environment (getArgs)
import qualified Data.Set as Set

data Dir = Lft | Rght | Up | Down deriving (Show, Eq, Ord)

data Orientation = Horizontal | Vertical deriving (Show, Eq, Ord)

data Position = Pos {x :: Int
                    ,y :: Int
                    ,dir :: Dir} deriving (Show, Eq, Ord)

data AbsPosition = AbsPosition {xa :: Int
                               ,ya :: Int
                               ,ori :: Orientation} deriving (Show, Eq, Ord)
                                
toAbs :: Position -> AbsPosition
toAbs p =
  let
    ori = case (dir p) of
      Lft -> Horizontal
      Rght -> Horizontal
      Up -> Vertical
      Down -> Vertical
  in
    AbsPosition (x p) (y p) ori

type Board = [[Char]]

inBounds :: Position -> Board -> Bool
inBounds pos board =
  let
    n = length (board !! 0)
    m = length board
  in
    (x pos) >= 0 && (x pos) < n && (y pos) >= 0 && (y pos) < m

-- Get character at position
charAt :: Position -> Board -> Maybe Char
charAt pos board =
  if inBounds pos board
  then Just $ (board !! (y pos)) !! (x pos)
  else Nothing

-- Where can we go from here?
simpleMove :: Position -> Dir -> Position
simpleMove pos dir =
  let
    pos' = pos {dir=dir}
  in
    case dir of
      Down -> pos' {y=(y pos)+1}
      Up -> pos' {y=(y pos)-1}
      Lft -> pos' {x=(x pos)-1}
      Rght -> pos' {x=(x pos)+1}

perp :: Dir -> [Dir]
perp Lft = [Up,Down]
perp Rght = [Up,Down]
perp Up = [Lft, Rght]
perp Down = [Lft, Rght]

validDir :: Position -> Board -> Maybe Dir
validDir pos board =
  let
    lst = filter isValid' (perp (dir pos))
  in
    if (length lst) == 0
    then Nothing
    else Just (head lst)
  where
    isValid' dir =
      let
        mc = charAt (simpleMove pos dir) board
        c = case mc of
          Just a -> a
          Nothing -> '@'
      in
        if c == '@' then False else
        case dir of
          Lft -> (c == '-') || (isLetter c)
          Rght -> (c == '-') || (isLetter c)
          Up -> (c == '|') || (isLetter c)
          Down -> (c == '|') || (isLetter c)

move :: Position -> Board -> (Position, Maybe Char)
move pos board =
  let
    mc = charAt pos board
    char = case mc of
      Just a -> a
      Nothing -> '@'
    letter = if isLetter char
      then Just char
      else Nothing
    smp d = if inBounds (simpleMove pos d) board
      then (simpleMove pos d)
      else pos
    pos' =
      case char of
        '+' -> case (validDir pos board) of
                 Just d -> (smp d)
                 Nothing -> pos
        _ -> smp (dir pos)
  in
    (pos',letter)

firstOccurIdx :: Eq a => [a] -> a -> Int
firstOccurIdx lst c =
  let
    n = length lst
    idxs = zip (take n [0,1..]) lst
    pt = head $ filter (\t -> snd t == c) idxs
  in
    fst pt

starting :: Board -> Position
starting board = Pos {x=(firstOccurIdx (board !! 0) '|'), y=0, dir=Down}
  
nMoves :: Board -> Int -> Position
nMoves board n = nMoves' (starting board) n where
  nMoves' p 0 = p
  nMoves' p n =
    let
      (p',_) = move p board
    in
      nMoves' p' (n-1)

getXy :: Position -> (Int,Int)
getXy p = (x p, y p)

type GetState a = (Position, Maybe Char) -> a -> a

getCharIfLetter :: GetState [Char]
getCharIfLetter (p',mc) lst =
  case mc of
    Nothing -> lst
    Just c -> lst ++ [c]

-- Part 1 : traverse the board  and grab all letters
part1 :: Board -> [Char]
part1 board = traverseBoard board (starting board) getCharIfLetter [] Set.empty

accum :: GetState Int
accum _ val = val + 1

part2 :: Board -> Int
part2 board = traverseBoard board (starting board) accum 0 Set.empty

-- Generic-ish board traversal
traverseBoard :: Board -> Position -> GetState a -> a -> Set.Set AbsPosition -> a
traverseBoard board pos getState lst visited =
    let
      (p', mc) = move pos board
      lst' = getState (p',mc) lst
      visited' = Set.insert (toAbs pos) visited
    in
      if p' == pos || Set.member (toAbs p') visited
      then lst -- We're done here
      else traverseBoard board p' getState lst' visited'

main :: IO()
main =
  do
    args <- getArgs
    content <- readFile (args !! 0)
    let
      board = lines content
    print $ part1 board
    print $ part2 board
