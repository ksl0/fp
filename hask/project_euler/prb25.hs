fib :: [Int]
fib = 0 : 1: [a+b | (a,b) <- zip fib (tail fib)]
