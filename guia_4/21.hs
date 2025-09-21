pitagoras :: Int -> Int -> Int -> Int
pitagoras (-1) _ _ = 0
pitagoras m n r = sumatoriaInterna m n r + pitagoras (m-1) n r 

sumatoriaInterna :: Int -> Int -> Int -> Int
sumatoriaInterna m (-1) r = 0
sumatoriaInterna m n r = cumpleCondicion m n r + sumatoriaInterna m (n-1) r 

cumpleCondicion :: Int -> Int -> Int -> Int
cumpleCondicion m n r | m ^ 2 + n ^ 2 <= r ^ 2 = 1
                      | otherwise = 0