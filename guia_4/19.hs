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

esSumaInicialDePrimos :: Int -> Bool
esSumaInicialDePrimos 0 = False
esSumaInicialDePrimos 1 = False
esSumaInicialDePrimos n = esSumaInicialDePrimosDesde 2 n 

esSumaInicialDePrimosDesde :: Int -> Int -> Bool
esSumaInicialDePrimosDesde n m | m == 0 = True
                               | m < 0 = False
                               | esPrimo n = esSumaInicialDePrimosDesde (n+1) (m-n) 
                               | otherwise = esSumaInicialDePrimosDesde (n+1) m

