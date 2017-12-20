-- Strategy: read into vector, move based on vector
import Data.Char

data Dir = Lft | Rght | Up | Down deriving (Show, Eq)

data Position = Pos {x :: Int
                    ,y :: Int
                    ,dir :: Dir} deriving (Show, Eq)

type Board = [[Char]]

inBounds :: Position -> Board -> Bool
inBounds pos board =
  let
    n = length board
    m = length (board !! 0)
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
simpleMove pos dir = case dir of
      Down -> pos {y=(y pos)+1}
      Up -> pos {y=(y pos)-1}
      Lft -> pos {x=(x pos)-1}
      Rght -> pos {x=(x pos)+1}

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
  
part1 :: Board -> [Char]
part1 board = part1' board (starting board) [] where
  part1' board pos lst =
    let
      (p', mc) = move pos board
      lst' = case mc of
        Nothing -> lst
        Just c -> c:lst
    in
      if p' == pos
      then lst -- We're done here
      else part1' board p' lst'
