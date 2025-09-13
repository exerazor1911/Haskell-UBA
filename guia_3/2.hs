-- A)Funcion absoluto
absoluto :: Integer -> Integer
absoluto x | x < 0 = -x
           | otherwise = x

-- B)Funcion maximoAbsoluto
maximoAbsoluto :: Integer -> Integer -> Integer
maximoAbsoluto x n | absoluto x == absoluto n = x
                   | absoluto x > absoluto n = x
                   | otherwise = n

-- Helper max function
maximo' :: (Ord a) => a -> a -> a
maximo' a b | a >= b = a
            | otherwise = b


-- C)Funcion maximo3
maximo3 :: Integer -> Integer -> Integer -> Integer
maximo3 x y z = maximo' x (maximo' y z)

-- D)Funcion algunoEsCero con pattern matching
algunoEsCeroPattern :: Rational -> Rational -> Bool
algunoEsCeroPattern 0 _ = True
algunoEsCeroPattern _ 0 = True
algunoEsCeroPattern _ _ = False

-- D)Funcion algunoEsCero sin pattern matching
algunoEsCero :: Rational -> Rational -> Bool
algunoEsCero x y | x == 0 = True
                 | y == 0 = True
                 | otherwise = False
                 
-- E)AmbosSonCero con pattern matching
ambosSonCeroPattern :: Rational -> Rational -> Bool
ambosSonCeroPattern 0 0 = True
ambosSonCeroPattern _ _ = False

-- E)AmbosSonCero sin pattern matching
ambosSonCero :: Rational -> Rational -> Bool
ambosSonCero x y | x == 0 && y == 0 = True
                 | otherwise = False

-- F)enMismoIntervalo
enMismoIntervalo :: Double -> Double -> Bool 
enMismoIntervalo x y | x <= 3 && y <= 3 = True
                     | x > 3 && x <= 7 && y > 3 && y <= 7 = True
                     | x > 7 && y > 7 = True
                     | otherwise = False

-- F)sumaDistintos
sumaDistintos :: Integer -> Integer -> Integer -> Integer
sumaDistintos x y z | x == y && x == z = x
                    | x == y = y + z
                    | x == z = y + z 
                    | y == z = x + y
                    | otherwise = x + y + z

-- H)esMultiploDe
esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe x y | x == 0 = True
                 | y == 0 = False
                 | x `mod` y == 0 = True
                 | otherwise = False

-- I) digitoUnidades 
digitoUnidades :: Integer -> Integer
digitoUnidades n = n `mod` 10 

-- J) digitoDecenas
digitoDecenas :: Integer -> Integer
digitoDecenas n = n `mod` 100