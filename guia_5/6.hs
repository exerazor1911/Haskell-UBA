type Texto = [Char]
type Nombre = Texto
type Telefono = Texto
type Contacto = (Nombre,Telefono)
type ContactosTel = [Contacto]

elNombre :: Contacto -> Nombre
elNombre (nombre,telefono) = nombre

elTelefono :: Contacto -> Telefono
elTelefono (nombre,telefono) = telefono

enLosContactos :: Nombre -> ContactosTel -> Bool
enLosContactos _ [] = False
enLosContactos nombre (contacto:contactos) | elNombre contacto == nombre = True
                                           | otherwise = enLosContactos nombre contactos

agregarContacto :: Contacto -> ContactosTel -> ContactosTel
agregarContacto (nombre,telefono) [] = [(nombre,telefono)]
agregarContacto (nombre,telefono) (contacto:contactos) | elNombre contacto == nombre = (nombre,telefono) : contactos
                                                       | otherwise = contacto : agregarContacto (nombre,telefono) contactos

eliminarContacto :: Nombre -> ContactosTel -> ContactosTel
eliminarContacto _ [] = []
eliminarContacto nombre (contacto:contactos) | elNombre contacto == nombre = eliminarContacto nombre contactos
                                             | otherwise = contacto : eliminarContacto nombre contactos