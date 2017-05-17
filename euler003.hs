-- Largest prime factor
isPrime :: Int -> Bool
isPrime x = null [a | a <- [2..isqrt x], x `mod` a == 0]

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

primeFactorList :: Int -> [Int]
primeFactorList x = [a | a <- [2..isqrt x], isPrime a, x `mod` a == 0] 

lpf :: Int -> Int
lpf x = maximum [a | a <- primeFactorList x]

euler3 :: Int -> Int
euler3 x = lpf x
