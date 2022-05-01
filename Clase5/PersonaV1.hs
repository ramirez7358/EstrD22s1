module PersonaV1 
  (Persona, nacer, edad, nombre, apellido, crecer)
 where

data Persona = P String String   Int
              -- Nombre Apellido Edad
  {- INV.REP.: en (P n a e), se cumple que
      * e >= 0
  -}

nacer    :: String  -> String -> Persona
 -- PROP: dados un nombre y un apellido, describe una persona con ese nombre 
 --       y apellido y recién nacida (edad 0)
edad     :: Persona -> Int
nombre   :: Persona -> String
apellido :: Persona -> String
crecer   :: Persona -> Persona
 -- PROP: dada una persona, describe a la persona con una año más de edad

nacer n a          = P n a 0
edad (P _ _ e)     = e
nombre (P n _ _)   = n
apellido (P _ a _) = a
crecer (P n a e)   = P n a (e+1)

-- FEO, Inválido, NO RESPETA el invariante de representación
ejemplo :: Persona
ejemplo = P "Hola" "Mundo" (-10)

