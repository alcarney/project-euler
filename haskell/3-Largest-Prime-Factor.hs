{-
  The prime factors of 5,7,13 and 29

  What is the largest prime factor of the number 600851475143?
 -}

data Tree a = Leaf a
            | Branch (Tree a) (Tree a)
            deriving(Show, Eq)

-- Is f a factor of the number n?
isFactor :: Int -> Int -> Bool
isFactor n f = n `rem` f == 0

-- Get a list of all the factors of n
getFactors :: Int -> [Int]
getFactors n = [f | f <- [n, n-1..1], isFactor n f]

-- Is this number prime?
-- Since a prime number's factors will be 1 and itself
-- it's prime if the length of factors is exactly 2
isPrime :: Int -> Bool
isPrime n = length (getFactors n) == 2

-- This implements the 'prime tree' factorization we learn in school
factor :: Int -> Int -> Tree Int
factor n f
    | f >= n = Leaf f
    | isFactor n f = Branch (factor f 2) (factor (n `div` f) 2)
    | otherwise = factor n (f + 1)

-- Do the problem
main :: IO ()
main = print $ factor 600851475143 2
