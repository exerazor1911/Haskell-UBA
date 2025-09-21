tomaValorMax :: Int -> Int -> Int
tomaValorMax n m | n == m = n
                 | sumaDivisores n n >= sumaDivisores m m = tomaValorMax n (m-1)
                 | otherwise = tomaValorMax (n+1) m

sumaDivisores :: Int -> Int -> Int
sumaDivisores n 1 = 1
sumaDivisores n m | esDivisor n m = m + sumaDivisores n (m-1)
                  | otherwise = sumaDivisores n (m-1)

esDivisor :: Int -> Int -> Bool
esDivisor n 1 = True
esDivisor n m = mod n m == 0