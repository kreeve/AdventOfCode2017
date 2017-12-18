data LockState = LockState Int Int Int [Int] deriving (Show,Eq)

insertAt :: Int -> a -> [a] -> [a]
insertAt pos val lst =
  let
    front = take pos lst
    back = drop pos lst
  in
    front ++ [val] ++ back

insertAfter :: Int -> a -> [a] -> [a]
insertAfter pos val lst =
  let
    n = length lst
  in
    if pos < n-1
    then insertAt (pos+1) val lst
    else lst ++ [val]

update :: LockState -> LockState
update (LockState jmp pos val lst) =
  let
    n = length lst
    lst' = take ((jmp+pos+1)*n) (cycle (take n [0,1..]))
    pos' = lst' !! (pos + jmp)
  in
    LockState jmp (pos'+1) (val+1) (insertAfter pos' (val+1) lst)

idxOf :: Eq a => [a] -> a -> Int
idxOf lst val = idxOf' lst 0
  where
    idxOf' [] _ = error $ "Not found"
    idxOf' (x:xs) i = if x == val
      then i
      else idxOf' xs (i+1)

listPart :: LockState -> [Int]
listPart (LockState _ _ _ seq) = seq

iterNum :: (a -> a) -> a -> Int -> a
iterNum f val 0 = val
iterNum f val n = iterNum f (f val) (n-1)

solve :: LockState -> Int -> Int -> Int
solve state iters val =
  let
    finalState = last $ take iters $ iterate update state
    idx = idxOf (listPart finalState) val
  in
    (listPart finalState) !! (idx+1)

createState :: Int -> LockState
createState jmp = LockState jmp 0 0 [0]

part1 :: LockState -> Int
part1 state = solve state 2018 2017

-- Part 2 : Next int after 0 after n iterations
part2 :: Int -> Int -> Int
part2 jmp n = part2' 1 0 0 where
  part2' i pos val =
    let
      pos' = ((pos+jmp) `mod` i) + 1
      val' = if pos' == 1
        then i
        else val
    in
      if i == n
      then val
      else (part2' (i+1) $! pos') val'
      
    
