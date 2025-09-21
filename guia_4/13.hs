sumatoriaInterna :: Int -> Int -> Int
sumatoriaInterna n 1 = n
sumatoriaInterna n m = n ^ m + sumatoriaInterna n (m-1) 

sumatoria :: Int -> Int -> Int
sumatoria 1 m = m
sumatoria n m = sumatoriaInterna n m + sumatoria (n-1) m