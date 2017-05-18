-- Smallest multiple
euler5 :: Int -> Int
euler5 x = smallestMultiple x x

smallestMultiple :: Int -> Int -> Int
smallestMultiple x a 
    | divisibleByAll a [2..x] == False = smallestMultiple x (a + x) 
    | otherwise = a

divisibleByAll :: Int -> [Int] -> Bool
divisibleByAll a [] = True
divisibleByAll a (x:xs) = (a `mod` x == 0) && (divisibleByAll a xs)
