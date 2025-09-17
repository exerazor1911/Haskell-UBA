todosDigitosIguales :: Integer -> Bool
todosDigitosIguales n | n < 10 = True
                      | otherwise = ultimoDigito == penultimoDigito && todosDigitosIguales (div n 10) where 
                        ultimoDigito = mod n 10
                        penultimoDigito = mod (div n 10) 10