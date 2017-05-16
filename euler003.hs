-- Largest prime factor

isPrime x = null [a | a <- [2..x], k `mod` x == 0]

primeList x = [a | a <- [2..x], isPrime a]

lpf :: Int -> Int
lpf x = 

euler3 :: Int -> Int
euler3 x = lpf x
