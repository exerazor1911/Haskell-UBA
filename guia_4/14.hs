sumaPotencias :: Int -> Int -> Int -> Int
sumaPotencias q 0 m = 0
sumaPotencias q n m = sumaPotenciasInterna q n m + sumaPotencias q (n - 1) m

sumaPotenciasInterna :: Int -> Int -> Int -> Int
sumaPotenciasInterna q n 1 = q ^ (n + 1)
sumaPotenciasInterna q n m = q ^ (n + m) + sumaPotenciasInterna q n (m - 1)
