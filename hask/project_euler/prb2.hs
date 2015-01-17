fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
ans = sum (filter (even) (takeWhile (<4000000) fibs))
