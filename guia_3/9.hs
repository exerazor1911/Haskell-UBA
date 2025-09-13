-- A
f1 :: Float -> Float
f1 n | n == 0 = 1
     | otherwise = 0
-- la funcion lo que hace es recibir 1 numero real que si es igual a 0 devuelve 1, sino devuelve 0
-- B
f2 :: Float -> Float
f2 n | n == 1 = 15
     | n == -1 = -15
-- la funcion lo que hace es recibir un numero real que si es igual a 1 devuelve 15, y si es igual a -1 devuelve -15
-- C
f3 :: Float -> Float
f3 n | n <= 9 = 7
     | n >= 3 = 5
-- la funcion lo que hace es recibir un numero real que si es menor o igual a 9 devuelve 7 y si es mayor o igual a 3 devuelve 5
-- D
f4 :: Float -> Float -> Float
f4 x y = ( x + y )/2
-- la funcion lo que hace es recibir 2 numeros reales, y a su suma dividirla por 2 y devolver dicho resultado
-- E
f5 :: (Float, Float) -> Float
f5 (x, y) = (x+y)/2
-- la funcion lo que hace es recibir una tupla de reales, de la cual suma sus elementos y devuelve dicha suma dividia por 2
-- F
f6 :: Float -> Int -> Bool
f6 a b = truncate a == b
-- la funcion lo que hace es recibir un real y un entero y se fija si sacandole la parte fraccionaria al real, el numero resultante es igual al entero recibido por parametro