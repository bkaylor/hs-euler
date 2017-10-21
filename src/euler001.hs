-- Multiples of 3 and 5
euler1 :: Int -> Int
euler1 y = sum [x | x <- [1..y-1], x `mod` 3 == 0 || x `mod` 5 == 0]
