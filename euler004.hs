-- Largest palindrome product
euler4 :: Int -> Int
euler4 x = largestPalindromeProduct x

largestPalindromeProduct :: Int -> Int
largestPalindromeProduct x = maximum [a * b | a <- [i..j], b <- [i..j], isPalindrome (a * b)]
    where i = smallestXDigitNumber x
          j = largestXDigitNumber x

smallestXDigitNumber :: Int -> Int
smallestXDigitNumber x = 10 ^ (x - 1)

largestXDigitNumber :: Int -> Int
largestXDigitNumber x = (10 ^ x) - 1

isPalindrome :: Int -> Bool
isPalindrome x = show x == reverse (show x)
