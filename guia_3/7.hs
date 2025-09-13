-- Helper
abs' :: (Ord a, Num a) => a -> a
abs' x | x >= 0 = x
       | otherwise = -x

-- distanciaManhattan
distanciaManhattan :: (Float, Float, Float) -> (Float, Float, Float) -> Float
distanciaManhattan (p1, p2, p3) (q1, q2, q3) = abs' (p1 - q1) + abs' (p2 - q2) + abs' (p3 - q3)

type Punto3D = (Float, Float, Float)
-- distanciaManhattanConType
distanciaManhattanConType :: Punto3D -> Punto3D -> Float
distanciaManhattanConType (p1, p2, p3) (q1, q2, q3) = abs' (p1 - q1) + abs' (p2 - q2) + abs' (p3 - q3)