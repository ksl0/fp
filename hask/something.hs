lucky :: (Integral a) => a -> String
lucky 7 = "Something"
lucky x = "nope"

factorial :: (Integral a) => a ->a
factorial 0 = 1
factorial n = n*factorial (n-1)


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
  let smallerSorted = quicksort [a | a <-xs, a <= x]
      biggerSorted  = quicksort [a | a <- xs, a >=x]
  in  smallerSorted ++ [x] ++ biggerSorted
