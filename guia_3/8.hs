-- Helper
abs' :: (Ord a, Num a) => a -> a
abs' x | x >= 0 = x
       | otherwise = -x

--comparar
comparar :: Integer -> Integer -> Integer
comparar a b | sumaUltimosDosDigitos a < sumaUltimosDosDigitos b = 1
             | sumaUltimosDosDigitos a > sumaUltimosDosDigitos b = -1
             | otherwise = 0

--sumaUltimosDosDigitos
sumaUltimosDosDigitos :: Integer -> Integer
sumaUltimosDosDigitos x = abs' x `mod` 10 + abs' x `div` 10 `mod` 10

