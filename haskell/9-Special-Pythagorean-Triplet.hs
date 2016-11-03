{-
 There exists exactly one pythagorean triplet for which
 a + b + c = 1000

 Find the product abc
 -}

nats :: [Int]
nats = [1..500]

triangles :: [(Int, Int, Int)]
triangles = [(a, b, c) | a <- nats, b <- nats, c <- nats]

righttris :: [(Int, Int, Int)]
righttris = [(a, b, c) | (a, b, c) <- triangles, a^2 + b^2 == c^2]

special :: [(Int, Int, Int)]
special = [(a, b, c) | (a, b, c) <- righttris, a + b + c == 1000]

sums :: [(Int, Int, Int)]
sums = [(a, b, c) | a <- nats, b <- nats, c <- nats , a + b + c == 1000, a^2 + b^2 == c^2]

main = print $ 200 * 375 * 425

