-- Sum square difference
euler6 :: Int -> Int
euler6 x = sumSquareDifference x

sumSquareDifference :: Int -> Int
sumSquareDifference x = ((sum naturals) ^ 2) - (sum (map (^ 2) naturals))
    where naturals = [1..x]
