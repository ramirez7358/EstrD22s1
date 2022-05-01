module PersonaV2 
  (Persona, nacer, edad, nombre, apellido, crecer)
 where

data Persona = PP String          Int
               -- NombreYApellido Edad
  {- INV.REP.: en (PP na e)
      * e >= 0
      * na tiene exactamente un espacio
  -}               

nacer    :: String  -> String -> Persona
 -- PROP: dados un nombre y un apellido, describe una persona con ese nombre 
 --       y apellido y recién nacida (edad 0)
 -- PRECOND: ni el nombre ni el apelldio deben tener espacios
edad     :: Persona -> Int
nombre   :: Persona -> String
apellido :: Persona -> String
crecer   :: Persona -> Persona
 -- PROP: dada una persona, describe a la persona con una año más de edad

nacer    n a       = 
    if (elem ' ' n) && (elem ' ' a)
     then error "No se pueden construir personas con nombres o apellidos compuestos"
     else PP (n ++ " " ++ a) 0
edad     (PP _ e)  = e
nombre   (PP na _) = obtenerHastaElEspacio na
apellido (PP na _) = obtenerDesdeElEspacio na
crecer   (PP na e) = PP na (e+1)

obtenerHastaElEspacio :: String -> String
 -- PROP: dado un nombre y apellido, extrae lo que hay antes del primer espacio
 -- PRECOND: hay un espacio en el string dado
obtenerHastaElEspacio []     = error "No hay un espacio"
obtenerHastaElEspacio (c:cs) =
    if (c == ' ')
     then ""  -- []   -- ' ' :: Char, no String
     else c : obtenerHastaElEspacio cs
--obtenerHastaElEspacio (' ':_) = ""
--obtenerHastaElEspacio (c:cs)  = c : obtenerHastaElEspacio cs     

{-
    obtenerHastaElEspacio "Fidel ML"
      /                      \
   'F'                 obtenerHastaElEspacio "idel ML"
                             |
                          "idel"
      \                     /
          'F' : "idel"
            "Fidel"
-}

obtenerDesdeElEspacio :: String -> String
 -- PROP: dado un nombre y apellido, extrae lo que hay después del primer espacio
 -- PRECOND: hay un espacio en el string dado
obtenerDesdeElEspacio []     = error "No hay un espacio"
obtenerDesdeElEspacio (c:cs) =
    if (c == ' ')
     then cs
     else obtenerDesdeElEspacio cs
