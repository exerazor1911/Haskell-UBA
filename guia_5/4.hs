sacarBlancosRepetidos :: [Char] -> [Char]
sacarBlancosRepetidos [] = []
sacarBlancosRepetidos [x] = [x]
sacarBlancosRepetidos (x : xs)
  | x == ' ' && head xs == ' ' = sacarBlancosRepetidos xs
  | otherwise = x : sacarBlancosRepetidos xs

-- HELPERS
contarEspaciosEntrePalabras :: [Char] -> Int
contarEspaciosEntrePalabras [] = 0
contarEspaciosEntrePalabras [x] = 0
contarEspaciosEntrePalabras a
  | head sinBlancosRepetidosNiExtremos /= ' ' && head (tail sinBlancosRepetidosNiExtremos) == ' ' = 1 + contarEspaciosEntrePalabras colaSinBlancosRepetidosNiExtremos
  | otherwise = contarEspaciosEntrePalabras colaSinBlancosRepetidosNiExtremos
  where
    sinBlancosRepetidosNiExtremos = sacarBlancosRepetidos (sacarBlancosDeExtremos a)
    colaSinBlancosRepetidosNiExtremos = sacarBlancosRepetidos (sacarBlancosDeExtremos (tail a))

sacarPrimerosBlancos :: [Char] -> [Char]
sacarPrimerosBlancos [] = []
sacarPrimerosBlancos [x]
  | x == ' ' = []
  | otherwise = [x]
sacarPrimerosBlancos (x : xs)
  | x == ' ' = sacarPrimerosBlancos xs
  | otherwise = x : xs

reverso :: [t] -> [t]
reverso [] = []
reverso [x] = [x]
reverso (x : xs) = reverso xs ++ [x]

sacarUltimosBlancos :: [Char] -> [Char]
sacarUltimosBlancos a = reverso (sacarPrimerosBlancos (reverso a))

sacarBlancosDeExtremos :: [Char] -> [Char]
sacarBlancosDeExtremos a = sacarUltimosBlancos (sacarPrimerosBlancos a)

sumarSiTieneElemento :: [Char] -> Int
sumarSiTieneElemento [] = 0
sumarSiTieneElemento x = 1

-- SIGUE ACA
contarPalabras :: [Char] -> Int
contarPalabras [] = 0
contarPalabras a
  | contarEspaciosEntrePalabras sinBlancosDeExtremos == 0 = sumarSiTieneElemento sinBlancosDeExtremos
  | otherwise = 1 + contarEspaciosEntrePalabras sinBlancosDeExtremos
  where
    sinBlancosDeExtremos = sacarBlancosDeExtremos a

palabras :: [Char] -> [[Char]]
palabras [] = []
palabras (x : xs)
  | x == ' ' = []
  | otherwise = crearListaDePalabras listaFormateada
  where
    listaFormateada = sacarBlancosRepetidos (sacarBlancosDeExtremos (x : xs))

crearListaDePalabras :: [Char] -> [[Char]]
crearListaDePalabras [] = [[]]
crearListaDePalabras (c : cs)
  | c /= ' ' = (c : head (crearListaDePalabras cs)) : tail (crearListaDePalabras cs)
  | otherwise = [] : crearListaDePalabras cs

palabraMasLarga :: [Char] -> [Char]
palabraMasLarga [] = []
palabraMasLarga a = obtenerPalabraMasLarga (palabras a)

-- HELPERS
obtenerPalabraMasLarga :: [[Char]] -> [Char]
obtenerPalabraMasLarga [] = []
obtenerPalabraMasLarga (x : xs)
  | longitud x > longitud (obtenerPalabraMasLarga xs) = x
  | otherwise = obtenerPalabraMasLarga xs

longitud :: [t] -> Int
longitud [] = 0
longitud (x : xs) = 1 + longitud xs

-- Preguntar si hace falta sacar los blancos del final
aplanar :: [[Char]] -> [Char]
aplanar [] = []
aplanar (x : xs) = x ++ aplanar xs

aplanarConBlancos :: [[Char]] -> [Char]
aplanarConBlancos [] = []
aplanarConBlancos (x : xs) = x ++ [' '] ++ aplanarConBlancos xs

aplanarConNBlancos :: [[Char]] -> Int -> [Char]
aplanarConNBlancos [] _ = []
aplanarConNBlancos (x : xs) n = x ++ agregarNBlancos n ++ aplanarConNBlancos xs n

agregarNBlancos :: Int -> [Char]
agregarNBlancos 1 = [' ']
agregarNBlancos n = [' '] ++ agregarNBlancos (n - 1)

-- con type Texto = [Char] cambia asi, por ej:
type Texto = [Char]

obtenerPalabraMasLargaTexto :: [Texto] -> Texto
obtenerPalabraMasLargaTexto [] = []
obtenerPalabraMasLargaTexto (x : xs)
  | longitud x > longitud (obtenerPalabraMasLargaTexto xs) = x
  | otherwise = obtenerPalabraMasLargaTexto xs