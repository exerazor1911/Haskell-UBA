import System.Posix (rename)
import TripletUtils

-- A)
productoInterno :: (Double, Double) -> (Double, Double) -> Double
productoInterno a b = fst a * fst b + snd a * snd b

-- B)
esParMenor :: (Double, Double) -> (Double, Double) -> (Bool, Bool)
esParMenor a b = (fst a < fst b, snd a < snd b) 

-- C)
distancia :: (Double, Double) -> (Double, Double) -> Double
distancia a b = sqrt((fst b - fst a) *  (fst b - fst a) + (snd b - snd a) * (snd b - snd a))

-- D)
sumaTerna :: (Integer, Integer, Integer) -> Integer
sumaTerna a = getFirstElement a + getSecondElement a + getThirdElement a

-- E)
sumarSoloMultiplos :: (Integer, Integer, Integer) -> Integer -> Integer 
sumarSoloMultiplos a b | b == 0     = error "Division by zero is not allowed"
                       | getFirstElement a `mod` b == 0 &&  getSecondElement a `mod` b == 0 && getThirdElement a `mod` b == 0 = getFirstElement a + getSecondElement a + getThirdElement a
                       | getFirstElement a `mod` b == 0 &&  getSecondElement a `mod` b == 0 = getFirstElement a + getSecondElement a
                       | getFirstElement a `mod` b == 0 && getThirdElement a `mod` b == 0 = getFirstElement a + getThirdElement a 
                       | getSecondElement a `mod` b == 0 && getThirdElement a `mod` b == 0 = getSecondElement a + getThirdElement a
                       | otherwise = 0

--helper
even' :: Integral a => a ->  Bool
even' x = x `mod` 2 == 0

-- F)
posPrimerPar :: (Integer, Integer, Integer) -> Integer
posPrimerPar a | even' (getFirstElement a) = 1
               | even' (getSecondElement a) = 2
               | even' (getThirdElement a) = 3
               | otherwise = 4

-- G) 
crearPar :: a -> b -> (a, b)
crearPar a b = (a, b)

-- H)
invertir :: a -> b -> (b, a)
invertir a b = (b, a)

-- I)
--productoInterno
type Punto2D = (Float, Float)
productoInternoConType :: Punto2D -> Punto2D -> Float
productoInternoConType a b = fst a * fst b + snd a * snd b

--esParMenor
esParMenorConType :: Punto2D -> Punto2D -> (Bool, Bool)
esParMenorConType a b = (fst a < fst b, snd a < snd b) 

--distancia
distanciaConType :: Punto2D -> Punto2D -> Float
distanciaConType a b = sqrt((fst b - fst a) *  (fst b - fst a) + (snd b - snd a) * (snd b - snd a))