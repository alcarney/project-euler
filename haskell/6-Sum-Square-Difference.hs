module Main where
-- The sum of squares of the first ten natural numbers is
--   1^2 + 2^2 + ... + 10^2 = 385
-- The square of the sum of the first ten natural numbers is
--   (1 + 2 + ... + 10)^2 = 3025
-- Hence the difference between the sum of squares of the
-- first ten natural numbers and the square of the sum
-- is 3025 - 2640
--
-- Find the difference between the sum of the squares of the
-- first one hundred natural numbers and the square of their
-- sum.

nats :: [Int]
nats = [1..]

squares :: [Int]
squares = map (\x -> x*x) nats

main :: IO ()
main = do
  putStr "Please enter the number of naturals you would like to sum til: "
  s <- getLine
  let i = read s :: Int

  let sumSquared = (\x -> x*x) $ sum $ take i nats
  let sumSquares = sum $ take i squares

  print (sumSquared - sumSquares)
