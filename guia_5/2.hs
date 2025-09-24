pertenece :: Eq t => t -> [t] -> Bool
pertenece e [] = False
pertenece e (x:xs) = x == e || pertenece e xs

todosIguales :: Eq t => [t] -> Bool
todosIguales [x] = True
todosIguales (x:xs) = x == head xs && todosIguales xs

todosDistintos :: Eq t => [t] -> Bool
todosDistintos [] = True
todosDistintos [x] = True
todosDistintos (x:xs) = esDistintoConLosProximos x xs  && todosDistintos xs

esDistintoConLosProximos :: Eq t => t -> [t] -> Bool
esDistintoConLosProximos n [x] = n /= x  
esDistintoConLosProximos n x = n /= head x && esDistintoConLosProximos n (tail x)

hayRepetidos :: Eq t => [t] -> Bool
hayRepetidos [] = False
hayRepetidos [x] = False
hayRepetidos (x:xs) = esIgualConLosProximos x xs || hayRepetidos xs

esIgualConLosProximos :: Eq t => t -> [t] -> Bool
esIgualConLosProximos n [x] = n == x  
esIgualConLosProximos n x = n == head x || esIgualConLosProximos n (tail x)

quitar :: Eq t => t -> [t] -> [t]
quitar n [] = []
quitar n [x] | n == x = []
             | otherwise = [x]
quitar n (x:xs) | n == x = xs 
                | otherwise = x : quitar n xs 

quitarTodos :: Eq t => t -> [t] -> [t] 
quitarTodos n [] = []
quitarTodos n [x] | n == x = []
                  | otherwise = [x]
quitarTodos n (x:xs) | n == x =  quitarTodos n xs
                     | otherwise = x : quitarTodos n xs

eliminarRepetidos :: Eq t => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos [x] = [x]
eliminarRepetidos (x:xs) = x : eliminarRepetidos (quitarTodos x xs)


mismosElementos :: Eq t => [t] -> [t] -> Bool
mismosElementos [] [] = True
mismosElementos [] _ = False
mismosElementos _ [] = False
mismosElementos a b = mismosElementos (tail a) (quitarTodos (head a) b)

reverso :: [t] -> [t]
reverso [] = []
reverso [x] = [x]
reverso (x:xs) = reverso xs ++ [x]

capicua :: Eq t => [t] -> Bool
capicua [] = True
capicua a = a == reverso a