esDivisible :: Int -> Int -> Bool
esDivisible n m | n == 0 = True
                | n < 0 = False
                | otherwise = esDivisible (n - m) m