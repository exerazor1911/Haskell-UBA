raizDe2Aprox :: Int -> Float
raizDe2Aprox n = raizDe2 n - 1

raizDe2 :: Int -> Float
raizDe2 1 = 2
raizDe2 n = 2 + 1/ raizDe2 (n-1)
