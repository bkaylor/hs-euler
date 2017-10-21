module Solutions
    ( euler1,
      euler2,
      euler3,
      euler4,
      euler5,
      euler6,
      euler7,
      --euler8
    ) where

import Utilities

import System.IO -- 8
import Data.Char -- 8

--------------------------
-- Multiples of 3 and 5
euler1 :: Int -> Int
euler1 y = sum [x | x <- [1..y-1], x `mod` 3 == 0 || x `mod` 5 == 0]

--------------------------
-- Even Fibonacci numbers
euler2 :: Int -> Int
euler2 y = sum [x | x <- takeWhile (< y) (buildFib 0 1), even x]

buildFib :: Int -> Int -> [Int]
buildFib a b = a : buildFib b (a+b) 

--------------------------
-- Largest prime factor
euler3 :: Int -> Int
euler3 x = largestPrimeFactor x

largestPrimeFactor :: Int -> Int
largestPrimeFactor x
    | isPrime x = x
    | otherwise = largestPrimeFactor (x `div` firstPrimeFactor x)

firstPrimeFactor :: Int -> Int
firstPrimeFactor x = head [a | a <- [2..isqrt x], x `mod` a == 0, isPrime a]

------------------------------
-- Largest palindrome product
euler4 :: Int -> Int
euler4 x = largestPalindromeProduct x

largestPalindromeProduct :: Int -> Int
largestPalindromeProduct x = maximum [a * b | a <- [i..j], b <- [i..j], isPalindrome (a * b)]
    where i = smallestXDigitNumber x
          j = largestXDigitNumber x

isPalindrome :: Int -> Bool
isPalindrome x = show x == reverse (show x)

smallestXDigitNumber :: Int -> Int
smallestXDigitNumber x = 10 ^ (x - 1)

largestXDigitNumber :: Int -> Int
largestXDigitNumber x = (10 ^ x) - 1

------------------------------
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

------------------------------
-- Sum square difference
euler6 :: Int -> Int
euler6 x = sumSquareDifference x

sumSquareDifference :: Int -> Int
sumSquareDifference x = ((sum naturals) ^ 2) - (sum (map (^ 2) naturals))
    where naturals = [1..x]

------------------------------
-- 10001st prime
euler7 :: Int -> Int
euler7 x = nthPrime x

nthPrime :: Int -> Int
nthPrime n = last ((take n) [a | a <- [2..], isPrime a])

------------------------------
-- Largest product in a series
-- euler8 :: Int -> Int
-- euler8 x = readFile file >>= largestProduct x
--     where file = "../misc/euler8_resource.txt"
-- 
-- largestProduct :: Int -> String -> Int
-- largestProduct x contents = maximum (map f [0..(length contents) - x])
--     where f = getRunningProduct x contents
-- 
-- getRunningProduct :: Int -> String -> Int -> Int
-- getRunningProduct x contents start = product (map ord (take x (drop start contents)))

