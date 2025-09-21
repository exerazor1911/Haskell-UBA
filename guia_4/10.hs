f1 :: Int -> Int
f1 0 = 1
f1 n = 2 ^ n + f1 (n - 1)


f2 :: Int -> Float -> Float
f2 1 q = q
f2 n q = q ^ n + f2 (n-1) q

f3 :: Int -> Float -> Float
f3 n m = f2 (2 * n) m

f4 :: Int -> Float -> Float
f4 n m = f4Helper (2*n) m n


f4Helper :: Int -> Float -> Int -> Float
f4Helper n m q | n == q = m ^ q
               | otherwise = m ^ n + f4Helper (n-1) m q