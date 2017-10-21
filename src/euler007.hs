-- 10001st prime
euler7 :: Int -> Int
euler7 x = nthPrime x

nthPrime :: Int -> Int
nthPrime n = last ((take n) [a | a <- [2..], isPrime a])

isPrime :: Int -> Bool
isPrime x = null [a | a <- [2..isqrt x], x `mod` a == 0]

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral
