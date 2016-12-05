{-
215 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

What is the sum of the digits of the number 2^1000?
-}

digitSum :: Integer -> Integer
digitSum n = sum digits
    where digits = map (\c -> read [c]) (show n)

main :: IO ()
main = print . digitSum $ 2^1000
