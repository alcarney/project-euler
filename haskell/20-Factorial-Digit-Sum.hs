{-
 n! means n x (n-1) x ... x 3 x 2 x 1

 For example 10! = 10 x 9 x 8 x ... x 2 x 1 = 3628800
 and the sum of the digits in the number 10! is 27

 Find the sum of the digits in the number 100!
 -}

factorial :: Integer -> Integer
factorial n = product [1..n]

sumDigits :: Integer -> String -> Integer
sumDigits acc [] = acc
sumDigits acc (c:cs) = sumDigits acc' cs
    where acc' = acc + read [c] :: Integer

main :: IO ()
main = print $ sumDigits 0 (show $ factorial 100)
