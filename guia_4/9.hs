esCapicua :: Int -> Bool
esCapicua n | n < 10 = True
            | otherwise =  esCapicuaDesdeHasta n 1 (cantDigitos n)

esCapicuaDesdeHasta :: Int -> Int -> Int -> Bool
esCapicuaDesdeHasta n a b | b <= a = True
                          | otherwise = iesimoDigito n a == iesimoDigito n b  && esCapicuaDesdeHasta n (a + 1) (b - 1) 

cantDigitos :: Int -> Int
cantDigitos n | n < 10 = 1
              | otherwise = 1 + cantDigitos (div n 10)

cortarUnidad :: Int -> Int
cortarUnidad 0 = 0
cortarUnidad x = div x 10

iesimoDigito :: Int -> Int -> Int
iesimoDigito n i | cantDigitos n == i = mod n 10
                 | otherwise = iesimoDigito (cortarUnidad n) i


-- RESOLVER CASO esCapicua 09990 = false -> tratandolo como int no se puede, te corta haskell el primer 0