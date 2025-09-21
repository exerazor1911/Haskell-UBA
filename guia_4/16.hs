-- A
menorDivisor :: Int -> Int 
menorDivisor 1 = 1
menorDivisor n = menorDivisorDesde n 2

menorDivisorDesde :: Int -> Int -> Int
menorDivisorDesde n m | mod n m == 0 = m
                      | otherwise = menorDivisorDesde n (m+1)

-- B
esPrimo :: Int -> Bool
esPrimo n = menorDivisor n == n 

-- C
sonCoprimos :: Int -> Int -> Bool
sonCoprimos n m | n == m = False
                | n > m = mcd n m == 1 
                | n < m = mcd m n == 1

mcd :: Int -> Int -> Int
mcd 1 _ = 1
mcd n m | mod n m == 0 = m
        | otherwise = mcd m (mod n m)

-- D
nEsimoPrimo :: Int -> Int 
nEsimoPrimo n = nEsimoPrimoDesde 2 n

nEsimoPrimoDesde :: Int -> Int -> Int
nEsimoPrimoDesde n 0 = n-1
nEsimoPrimoDesde n m | esPrimo n = nEsimoPrimoDesde (n+1) (m-1) 
                     | otherwise = nEsimoPrimoDesde (n+1) m