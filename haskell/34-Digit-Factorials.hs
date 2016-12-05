{-
145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

Find the sum of all numbers which are equal to the sum of the factorial of their digits.

Note: as 1! = 1 and 2! = 2 are not sums they are not included.
-}

factorial :: Integer -> Integer
factorial n = product [1..n]

-- Given a number do the factorial sum of the digits
-- e.g. 12 -> 1! + 2! = 3
factorialSum :: Integer -> Integer
factorialSum n = sum ns
    where ns = map (\x -> (factorial . read) [x]) (show n)

isCurious :: Integer -> Bool
isCurious n = n == factorialSum n

curious :: Integer -> [Integer]
curious n = [m | m <- [3..n], isCurious m]

main :: IO ()
main = print $ sum $ curious 1000000

