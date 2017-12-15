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
genSeries :: Int -> Int -> [Int]
genSeries factor start = iterate (gen factor) start

-- Solve a generic problem
solve :: (Int->Bool) -> (Int->Bool) -> Int -> Int -> Int -> Int
solve acond bcond n astart bstart =
  let
    avals = filter acond $ genSeries 16807 astart
    bvals = filter bcond $ genSeries 48271 bstart
    cmps = map (uncurry compareVals) $ zip avals bvals
  in
    length $ filter (\x->x) (take n cmps)

-- Part 1: all vals
part1 :: Int -> Int -> Int
part1 = solve (\x->True) (\x->True) 40000000

-- Part 2: only mults of 4 and 8
part2 :: Int -> Int -> Int
part2 = solve (\x -> x `mod` 4 == 0) (\x -> x `mod` 8 == 0) 5000000
