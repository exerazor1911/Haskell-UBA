sumaImpares :: Int -> Int 
sumaImpares 1 = 1
sumaImpares n = sumarImpar + sumaImpares (n - 1) where sumarImpar = 2 * n - 1