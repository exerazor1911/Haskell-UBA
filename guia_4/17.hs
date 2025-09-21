esFibonacci :: Int -> Bool
esFibonacci n = esFibonacciDesde n 0

esFibonacciDesde :: Int -> Int -> Bool
esFibonacciDesde 0 _ = True
esFibonacciDesde n m | m > n = False
                     | otherwise = fibonacci m == n || esFibonacciDesde n (m+1)

-- fibonacci
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)