type Texto = [Char]
type Identificacion = Int
type Ubicacion = Texto
type Estado = (Disponibilidad,Ubicacion)
type Locker = (Identificacion,Estado)
type MapaDeLockers = [Locker]
type Disponibilidad = Bool

existeElLocker :: Identificacion -> MapaDeLockers -> Bool
existeElLocker _ [] = False
existeElLocker id ((identificacion,estado):lockers) = identificacion == id || existeElLocker id lockers

ubicacionDelLocker :: Identificacion -> MapaDeLockers -> Ubicacion
ubicacionDelLocker id ((identi,(disponibilidad,ubicacion)):lockers) | id == identi = ubicacion
                                                                    | otherwise = ubicacionDelLocker id lockers

estaDisponibleElLocker :: Identificacion -> MapaDeLockers -> Bool
estaDisponibleElLocker id ((identi,(disponibilidad,ubicacion)):lockers) | id == identi = disponibilidad
                                                                        | otherwise = estaDisponibleElLocker id lockers

ocuparLocker :: Identificacion -> MapaDeLockers -> MapaDeLockers
ocuparLocker id ((identi,(disponibilidad,ubicacion)):lockers) | id == identi = (identi,(True,ubicacion)) : lockers
                                                              | otherwise = (identi,(disponibilidad,ubicacion)) : ocuparLocker id lockers