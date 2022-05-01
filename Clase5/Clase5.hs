--import PersonaV1
import PersonaV2 

import TermometroV1
--import TermometroV2


unoConTodos :: a -> [a] -> [(a,a)]  -- lineal
unoConTodos x []     = []
unoConTodos x (y:ys) = (x,y) : unoConTodos x ys
                    -- constante

todosConTodos :: [a] -> [(a,a)]     -- cuadrática
todosConTodos xs = cadaUnoConTodos xs xs
  where cadaUnoConTodos []     _     = []
        cadaUnoConTodos (x:xs) todos = 
            unoConTodos x todos ++ cadaUnoConTodos xs todos
            -- lineal


-- ============================
--  PERSONAS
-- ============================
ff :: Persona
ff = crecerVeces 53 (nacer "Fidel" "ML")

-- INVÁLIDO: P no es un elemento de la interfaz
--juan = P "Juan" "Perez" 25

-- INVÁLIDO: PP no es un elemento de la interfaz
--pedro = PP "Pedro Ruiz" 47

-- INVÁLIDO: obtenerHastaElEspacio no figura en la interfaz
-- ejemplo = obtenerHastaElEspacio "Hola Mundo!"

crecerVeces :: Int -> Persona -> Persona
crecerVeces 0 p = p
crecerVeces n p = crecer (crecerVeces (n-1) p)

nacerMuchas :: [(String,String)] -> [Persona]
nacerMuchas []          = []
nacerMuchas ((n,a):nas) = nacer n a : nacerMuchas nas

nombres :: [Persona] -> [String]
nombres []     = []
nombres (p:ps) = nombre p : nombres ps


-- ============================
--  TERMOMETROS
-- ============================
ingresarTemps       :: [Int] -> Termometro -> Termometro
ingresarTemps []     term = term
ingresarTemps (t:ts) term = ingresarT t (ingresarTemps ts term)

ultimasTempsDeTodos :: [Termometro] -> [Int]
-- PRECOND: ninguno de los termometros está vacío (sin temperaturas)
ultimasTempsDeTodos []           = []
ultimasTempsDeTodos (term:terms) = ultimaT term : ultimasTempsDeTodos terms

todasLasTemps       :: Termometro -> [Int]
todasLasTemps term =
    if (sinTempsT term)
     then []
     else ultimaT term : todasLasTemps (quitarUltimaT term)


-- NO SE PUEDE, PORQUE Termometro NO ES ALGEBRAICO
--todasLasTempeMAL nuevoT = []   -- ERROR: nuevoT NO ES un constructor
--todasLasTempsMAL term   = ultimaT term : todasLasTemps (quitarUltimaT term)

cuantasNegativas    :: Termometro -> Int
cuantasNegativas term =
    if (sinTempsT term)
     then 0
     else delta (ultimaT term < 0) + cuantasNegativas (quitarUltimaT term)

delta :: Bool -> Int  -- Función delta de Kröenecker
delta True  = 1
delta False = 0

dameNRecientes      :: Int -> Termometro -> [Int]
dameNRecientes = error "HACER"

{- Ejemplo de lista de máximos
temps:   [15,20, 5,10]
maximos: [20,20,10,10]
-}

