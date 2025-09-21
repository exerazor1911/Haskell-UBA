mayorDigitoPar :: Int -> Int
mayorDigitoPar n | n < 10 && esPar n = n
                 | n < 10 = -1
                 | esPar (mod n 10) && mod n 10 > mayorDigitoPar (div n 10) = mod n 10
                 | otherwise = mayorDigitoPar (div n 10)

esPar :: Int -> Bool
esPar n = mod n 2 == 0