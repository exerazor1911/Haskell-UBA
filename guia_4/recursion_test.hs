factorial :: (Eq a, Num a, Ord a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

esPar :: (Num a, Eq a) => a -> Bool
esPar 0 = True
esPar 1 = False
esPar n = esPar (n-2)


sumaLosPrimerosNImpares :: (Num a, Eq a) => a -> a
sumaLosPrimerosNImpares 1 = 1
sumaLosPrimerosNImpares n = obtenerImpar n + sumaLosPrimerosNImpares (n - 1) 
                            where obtenerImpar n = 2 * n - 1 
                        