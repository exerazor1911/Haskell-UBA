esCapicua :: Int -> Bool
esCapicua n = n < 10 || (primero == ultimo && esCapicua (sacarPrimeroYUltimo n)) where
    primero = iesimoDigito n 1
    ultimo = mod n 10

sacarPrimeroYUltimo :: Int -> Int
sacarPrimeroYUltimo n = eliminarUnidades (sacarNumeroMasSignificativo n)

sacarNumeroMasSignificativo :: Int -> Int
sacarNumeroMasSignificativo n = mod n (10 ^ (cantidadDeDigitos n - 1))

cantidadDeDigitos :: Int -> Int
cantidadDeDigitos x | x < 10 = 1
    | otherwise = 1 + cantidadDeDigitos (eliminarUnidades x)

eliminarUnidades :: Int -> Int
eliminarUnidades x = div x 10

iesimoDigito :: Int -> Int -> Int
iesimoDigito x i | i == cantidadDeDigitos x = digitoUnidades x
        | otherwise = iesimoDigito (eliminarUnidades x) i

digitoUnidades :: Int -> Int
digitoUnidades x = mod x 10