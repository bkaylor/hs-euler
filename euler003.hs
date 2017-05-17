-- Largest prime factor
isPrime :: Int -> Bool
isPrime x = null [a | a <- [2..isqrt x], x `mod` a == 0]

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

primeList :: Int -> [Int]
primeList x = [a | a <- [2..x-1], isPrime a]

lpf :: Int -> Int
lpf x = maximum [a | a <- primeList x, x `mod` a == 0]

euler3 :: Int -> Int
euler3 x = lpf x
