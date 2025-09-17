cantDigitos :: Int -> Int 
cantDigitos n | n < 10 = 1
cantDigitos n = 1 + cantDigitos (div n 10)

iesimoDigito :: Int -> Int -> Int
iesimoDigito n i | cantDigitos n == i = mod n 10
                 | otherwise = iesimoDigito (cortarUnidad n) i

cortarUnidad :: Int -> Int
cortarUnidad x = div x 10