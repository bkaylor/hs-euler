-- Even Fibonacci numbers
buildFib :: Int -> Int -> [Int]
buildFib a b = a : buildFib b (a+b) 

euler2 :: Int -> Int
euler2 y = sum [x | x <- takeWhile (< y) (buildFib 0 1), even x]
