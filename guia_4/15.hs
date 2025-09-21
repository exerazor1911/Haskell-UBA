sumaRacionales :: Int -> Int -> Float
sumaRacionales 0 _ = 0
sumaRacionales n m = sumaRacionalesInterna n m + sumaRacionales (n-1) m

sumaRacionalesInterna :: Int -> Int -> Float
sumaRacionalesInterna n 0 = 0
sumaRacionalesInterna n m = fromIntegral n / fromIntegral m + sumaRacionalesInterna n (m-1) 