-- Largest prime factor

isprime x = null [a | a <- [2..x], k `mod` x == 0]

prime-list x = [a | a <- [2..x], isprime a]

lpf :: Int -> Int
lpf x = 

euler3 :: Int -> Int
euler3 x = lpf x
