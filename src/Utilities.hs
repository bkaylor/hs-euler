module Utilities
    ( isqrt,
      isPrime
    ) where

isPrime :: Int -> Bool
isPrime x = null [a | a <- [2..isqrt x], x `mod` a == 0]
    
isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral
