eAprox :: Int -> Float
eAprox 0 = 1
eAprox n = 1 / fromIntegral (factorial n) + eAprox (n-1)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

e :: Float
e = eAprox 10