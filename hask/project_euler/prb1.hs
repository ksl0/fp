multiples3and5 :: Integer

multiples3and5 = sum [x | x <- [1, 2..1000], x `mod` 3==0 ||  x `mod` 5==0] 
