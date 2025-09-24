sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria [x] = x
sumatoria (x : xs) = x + sumatoria xs

productoria :: [Int] -> Int
productoria [] = 0
productoria [x] = x
productoria (x : xs) = x * productoria xs

maximo :: [Int] -> Int
maximo [] = 0
maximo [x] = x
maximo (x : xs)
  | x > maximo xs = x
  | otherwise = maximo xs

sumarN :: Int -> [Int] -> [Int]
sumarN 0 b = b
sumarN n [x] = [x + n]
sumarN n (x : xs) = (x + n) : sumarN n xs

sumarElPrimero :: [Int] -> [Int]
sumarElPrimero [] = []
sumarElPrimero a = sumarN (head a) a

reverso :: [t] -> [t]
reverso [] = []
reverso [x] = [x]
reverso (x : xs) = reverso xs ++ [x]

sumarElUltimo :: [Int] -> [Int]
sumarElUltimo [] = []
sumarElUltimo a = reverso (sumarElPrimero (reverso a))

esPar :: Int -> Bool
esPar a = mod a 2 == 0

pares :: [Int] -> [Int]
pares [] = []
pares (x : xs)
  | esPar x = x : pares xs
  | otherwise = pares xs

esMultiplo :: Int -> Int -> Bool
esMultiplo _ 0 = False
esMultiplo a b = mod a b == 0

multiplosDeN :: Int -> [Int] -> [Int]
multiplosDeN _ [] = []
multiplosDeN n a | esMultiplo (head a) n = head a : multiplosDeN n (tail a)
                 | otherwise = multiplosDeN n (tail a)

quitar :: Eq t => t -> [t] -> [t]
quitar n [] = []
quitar n [x] | n == x = []
             | otherwise = [x]
quitar n (x:xs) | n == x = xs 
                | otherwise = x : quitar n xs 

minimo :: [Int] -> Int
minimo [] = 0
minimo [x] = x
minimo (x : xs)
  | x < minimo xs = x
  | otherwise = minimo xs

ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar [x] = [x]
ordenar (x:xs) | x < minimo xs = x : ordenar xs
               | otherwise = minimo (x:xs) : ordenar (quitar (minimo (x:xs)) (x:xs))

            