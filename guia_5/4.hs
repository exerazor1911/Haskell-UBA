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