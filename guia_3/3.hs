estanRelacionados :: Integer -> Integer -> Bool
estanRelacionados a b | a == 0 || b == 0 = error "a o b no deben ser 0"
                      | (a * a) `mod` (a * b) == 0 = True
                      | otherwise = False