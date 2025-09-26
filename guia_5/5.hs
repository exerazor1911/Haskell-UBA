sumaAcumulada :: (Num t) => [t] -> [t]
sumaAcumulada [] = []
sumaAcumulada a = acumularDesde a 0

acumularDesde :: (Num t) => [t] -> t -> [t]
acumularDesde [] _ = []
acumularDesde (x : xs) n = (n + x) : acumularDesde xs (n + x)

descomponerEnPrimos :: [Int] -> [[Int]]
descomponerEnPrimos [] = []
descomponerEnPrimos (x : xs) = obtenerPrimosDivisores x : descomponerEnPrimos xs

obtenerPrimosDivisores :: Int -> [Int]
obtenerPrimosDivisores 2 = [2]
obtenerPrimosDivisores n = devolverDivisoresDe n (obtenerPrimosDesdeHasta 2 n)

obtenerPrimosDesdeHasta :: Int -> Int -> [Int]
obtenerPrimosDesdeHasta desde hasta
  | desde >= hasta = []
  | not (esPrimo desde) = obtenerPrimosDesdeHasta (desde + 1) hasta
  | otherwise = desde : obtenerPrimosDesdeHasta (desde + 1) hasta

devolverDivisoresDe :: Int -> [Int] -> [Int]
devolverDivisoresDe _ [] = []
devolverDivisoresDe num (x : xs)
  | esDivisor num x = x : devolverDivisoresDe num xs
  | otherwise = devolverDivisoresDe num xs

esDivisor :: Int -> Int -> Bool
esDivisor n m = mod n m == 0

esPrimo :: Int -> Bool
esPrimo 2 = True
esPrimo n = menorDivisor n == n

menorDivisor :: Int -> Int
menorDivisor n = menorDivisorDesde n 2

menorDivisorDesde :: Int -> Int -> Int
menorDivisorDesde 2 _ = 2
menorDivisorDesde num desde
  | mod num desde == 0 = desde
  | otherwise = menorDivisorDesde num (desde + 1)