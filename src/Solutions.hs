module Solutions
    ( euler1,
      euler2,
      euler3,
      euler4,
      euler5,
      euler6,
      euler7,
      euler8
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
-- TODO(bkaylor): How to make this testable? Research & implement.
euler8_alt :: Int -> IO Int
euler8_alt x = do
    contents <- readFile filename
    return (largestProduct x contents)
        where filename = "misc/euler8_resource.txt"
 
euler8 :: Int -> Int
euler8 x = largestProduct x s
    where s =  "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"

largestProduct :: Int -> String -> Int
largestProduct x contents = maximum (map f [0..(length contents) - x])
    where f = getRunningProduct x contents
 
getRunningProduct :: Int -> String -> Int -> Int
getRunningProduct x contents start = product (map ord (take x (drop start contents)))
