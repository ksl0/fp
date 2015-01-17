-- taken from website, need to learn about primes
divideUntilDone n a = divide2 n a 
  where
    divide2 n 1 = n
    divide2 n f
     | n `mod` f == 0  = divide2 (n `div` a) a 
     | otherwise = n  
     
primeFactors n = primeFactors' n 2
  where
    primeFactors' 1 _ = []
    primeFactors' n f 
     | n `mod` f == 0 = f : primeFactors' (divideUntilDone n f) f
     | otherwise      = primeFactors' n (f+1)

-- relatively inefficient way, not true 'sieve' 
primes :: [Integer]
primes = sieve [2..]
  where 
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p >0]



