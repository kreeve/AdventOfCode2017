type Pos = (Int, Int)
type Dir = (Int, Int)

north :: Dir
north = (0,-1)

south :: Dir
south = (0, 1)

east :: Dir
east = (1,0)

west :: Dir
west = (-1,0)

northeast :: Dir
northeast = (1,-1)

southeast :: Dir
southeast = (1,1)

northwest :: Dir
northwest = (-1,-1)

southwest :: Dir
southwest = (-1,1)


move :: Pos -> Dir -> Pos
move (x,y) (z,w) = (x+z, y+w)

allDirs :: [Dir]
allDirs = [north, south, east, west, northeast, southeast, northwest, southwest]

neighbors :: Pos -> [Pos]
neighbors pos = map (\d -> move pos d) allDirs

smallestVertex :: [(Pos,Int)] -> (Pos,Int)
smallestVertex q =
  let
    minVal = foldr1 min (map snd q)
  in
    head (filter (\x -> (snd x) == minVal) q)

contained :: [(Pos,Int)] -> Pos -> Bool
contained q p = case lookup p q of
  Nothing -> False
  Just _ -> True

inList :: Eq a => [a] -> a -> Bool
inList [] _ = True
inList (x:xs) y = (x /= y) && (inList xs y)

-- Shortest path from point a to point b
shortestPath :: (Pos,Int) -> Pos -> [(Pos,Int)] -> [Pos] -> Int
shortestPath (cur,cost) dest queue seen
  | cur == dest = cost
  | otherwise = 
    let
      seen' = seen ++ [cur]
      nbs = neighbors cur -- Our neighbors
      neighborDist = cost + 1
      toAdd = filter ((inList seen)) nbs
      nq = queue ++ map (\x -> (x,neighborDist)) toAdd
      next = smallestVertex nq
      nq' = filter (\x -> x /= next) nq
    in
      shortestPath next dest nq' seen'
      
    
