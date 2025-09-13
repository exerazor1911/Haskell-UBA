-- Función f
f :: Integer -> Integer
f 1 = 8
f 4 = 131
f 16 = 16

-- Función g
g :: Integer -> Integer
g n
  | n == 8 = 16
  | n == 16 = 4
  | n == 131 = 1

-- Funcion f o g
fog :: Integer -> Integer
fog n = f (g n)

-- Funcion g o f
gof :: Integer -> Integer
gof n = g (f n)

-- Nota: no agregue valor por defecto ocn otherwise ya que en este caso siento que seria sobreespecificar, consultar si esto esta ok.
