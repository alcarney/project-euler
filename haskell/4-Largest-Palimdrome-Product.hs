module Main where

-- A palindrome number reads the same both ways. The largest palindrome made
-- from the product of 2 digit numbers is 9009 = 91 x 99
--
-- Find the largest palindrome made from the product of two 3 digit numbers

threeDigits :: [Int]
threeDigits = [100..999]

palindromes :: [Int]
palindromes = filter (\n -> isPalindrome $ show n) [x * y | x <- threeDigits, y <- threeDigits]

isPalindrome :: String -> Bool
isPalindrome s = s == (reverse s)

main :: IO ()
main = putStrLn $ show $ maximum palindromes
  
