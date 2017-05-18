-- Largest prime factor
isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

isPrime :: Int -> Bool
isPrime x = null [a | a <- [2..isqrt x], x `mod` a == 0]

firstPrimeFactor :: Int -> Int
firstPrimeFactor x = head [a | a <- [2..isqrt x], x `mod` a == 0, isPrime a]

largestPrimeFactor :: Int -> Int
largestPrimeFactor x
    | isPrime x = x
    | otherwise = largestPrimeFactor (x `div` firstPrimeFactor x)

euler3 :: Int -> Int
euler3 x = largestPrimeFactor x
