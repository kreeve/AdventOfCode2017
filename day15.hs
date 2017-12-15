-- Compose a function n times with itself
compose :: Int -> (a->a) -> [(a->a)]
compose n f =  scanl (.) id (replicate n f)

-- Generate the next value
gen :: Int -> Int -> Int
gen factor val = (val*factor) `mod` (2^31-1)

-- Check if 2 vals last 16 bits match
compareVals :: Int -> Int -> Bool
compareVals v1 v2 = cmp' v1 == cmp' v2
  where
    cmp' v = v `mod` (2^16)

-- Generate the values
genSeries :: Int -> Int -> Int -> [Int]
genSeries factor start n =
  let
    f = scanl (.) start (replicate n (gen factor))
  in
    f

-- Part 1
part1 :: Int -> Int -> Int
part1 astart bstart =
  let
    n = 40000000
    avals = genSeries 16807 astart n
    bvals = genSeries 48271 bstart n
    cmps = map (uncurry compareVals) $ zip avals bvals
  in
    length $ filter (\x->x) cmps
    
