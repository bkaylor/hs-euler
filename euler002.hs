-- Even Fibonacci numbers
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibsum :: Int -> Int -> [Int] -> Int
fibsum i y xs
    | z > y          = sum xs
    | z `mod` 2 == 0 = fibsum (i+1) y (z:xs)
    | otherwise      = fibsum (i+1) y xs
    where z = fib i

euler2 :: Int -> Int
euler2 y = fibsum 0 y []
