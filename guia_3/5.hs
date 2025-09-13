--Funcion todoMenores
todoMenores :: (Integer, Integer, Integer) -> Bool
todoMenores (a,b,c) = (f a > g a) && (f b > g b) && (f c > g c)

f :: Integer -> Integer
f n | n <= 7 = n * n
    | otherwise = 2 * n - 1

g :: Integer -> Integer 
g n | even n = n `div` 2
    | otherwise = 3 * n + 1