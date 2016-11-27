{-
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
Find the sum of all the primes below two million.
-}

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

-- Drop all numbers which are divisible by p
sieve :: Int -> [Int] -> [Int] -> [Int]
sieve p acc [] = acc
sieve p acc (n:ns)
    | isFactor n p = sieve p acc ns
    | otherwise = sieve p (n:acc) ns

-- This takes the above function and generalises it to take a list
sieveList :: [Int] -> [Int] -> [Int]
sieveList [] ns = ns
sieveList (p:ps) ns = sieveList ps (sieve p [] ns)

-- We need a list of primes for the sieve to work with
seedPrimes :: Int -> [Int]
seedPrimes n = [p | p <- [2..n'], isPrime p]
    where n' = (floor . sqrt . fromIntegral ) n

-- Now we implement the prime sieve
-- Generates primes less than and equal to n
primeSieve :: Int -> [Int]
primeSieve n = ps ++ qs
    where ps = seedPrimes n
          qs = sieveList ps [2..n]

main :: IO ()
main = print $ sum $ primeSieve 2000000



