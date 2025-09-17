sumaDigitos :: Int -> Int
sumaDigitos 0 = 0
sumaDigitos n = obtenerUnidad + sumaDigitos cortarUnidad where 
    obtenerUnidad = mod n 10
    cortarUnidad = div n 10
