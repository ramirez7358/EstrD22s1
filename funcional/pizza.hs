-- data N = Z | S N, tal vez me convendría usar algo como N en lugar de Int
-- para aceituas

data B = T | F

not' :: B -> B
not' T = F
not' F = T

-- si esto es el and de los booleanos, si T se comporta como falso y F como verdadero...
and' :: B -> B -> B
and' F b2 = F
and' T b2 = b2

data NUC = Uno | Cero

not' :: B -> B
not' T = F
not' F = T

sig :: NUC -> NUC
sig Cero = Uno
sig Uno  = Cero

add :: NUC -> NUC -> NUC
add Cero Cero = Cero
add Cero Uno  = Uno
add Uno  Cero = Uno
add Uno  Uno  = Cero

orEx :: B -> B -> B
orEx F F = F
orEx F T = T
orEx T F = T
orEx T T = F

-- Para todo n
-- sig n = add n Uno

-- similar a aritmética modular, módulo 2

-- por diseño de álgebra
id = sig . sig

-- sig :: NUC -> NUC
-- sig Cero = Uno
-- sig Uno  = error "no tiene"

data Ingrediente = Aceitunas Int 
                 | Jamon 
                 | Queso 
                 | Salsa

data Pizza = Prepizza 
           | Capa Ingrediente Pizza

-- 1) Las reglas que definen
-- al conjunto inductivo Pizza son las siguientes:
-- caso base) Prepizza pertenece al conjunto
-- caso ind) Si i pertenece al conjunto Ingrediente y p al conjunto Pizza
--           entonces Capa i p pertenece al conjunto Pizza

-- Las reglas que definen al conjunto Ingrediente son:
-- caso 1) Si n pertenece a Int, entonces Aceitunas n pertence a Ingrediente
-- caso 2) Jamon pertenece al conjunto Ingrediente
-- caso 3) Queso pertenece al conjunto Ingrediente
-- caso 4) Salsa pertenece al conjunto Ingrediente

-- esquema de recursión estructural sobre Pizza
f :: Pizza -> ...
f Prepizza   = ... 
f (Capa i p) = ... i ... f p

cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza   = 0 
cantidadDeCapas (Capa i p) = 1 + cantidadDeCapas p

-- Alternativa 1: la que usa un if, no tan conveniente agregarlo
cantidadDeAceitunas :: Pizza -> Int
cantidadDeAceitunas Prepizza   = 0
cantidadDeAceitunas (Capa i p) = 
	if esAceitunas i
	   then aceitunas i + cantidadDeAceitunas p
	   else cantidadDeAceitunas p

esAceitunas (Aceituna n) = True
esAceitunas _            = False

-- Parcial en los demás ingrediente (dan bottom)
aceitunas (Aceituna n) = n
aceitunas _            = 0

-- Alternativa 2
-- (mal, en el sentido de no seguir el esquema recursivo estructural)
cantidadDeAceitunas :: Pizza -> Int
cantidadDeAceitunas Prepizza   = ... 
cantidadDeAceitunas (Capa (Aceitunas n) p) = n + cantidadDeAceitunas p
cantidadDeAceitunas (Capa i p) = cantidadDeAceitunas p

-- Alternativa final
cantidadDeAceitunas :: Pizza -> Int
cantidadDeAceitunas Prepizza   = 0
cantidadDeAceitunas (Capa i p) = aceitunas i + cantidadDeAceitunas p

-- función que cubre todos los casos
-- Precondición: las aceitunas tienen que ser >= 0
aceitunas :: Ingrediente -> Int
aceitunas (Aceitunas n) = n
aceitunas _             = 0

-- aceitunas i >= 0

-- Caso 1) Aceitunas n

-- max n 0 >= 0

-- ahora sí podría demostrar que siempre queda positivo

-- -- Sino...
-- -- Si el usuario utilizó un n para Aceitunas que sea positivo,
-- -- entonces sí, y sino no

-- Caso 2) cualquier otro ingrediente

-- 0 >= 0

-- Este caso es siempre cierto

-- Otra alternativa
-- cantidadDeAceitunas :: Pizza -> Int
-- cantidadDeAceitunas Prepizza   = 0
-- cantidadDeAceitunas (Capa i p) = 
--   (case i of  
--      	  (Aceitunas n) -> n
--      	  _             -> 0) + cantidadDeAceitunas p

-- comentario: sí es necesario el if en casos como este
-- max x y = if x >= y
--              then x
--              else y

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza   = Prepizza
duplicarAceitunas (Capa i p) = Capa (dupAcIng i) (duplicarAceitunas p)

dupAcIng (Aceituna n) = Aceituna (n*2)
dupAcIng i = i

-- Alternativa 1
sinLactosa :: Pizza -> Pizza
sinLactosa Prepizza   = Prepizza
sinLactosa (Capa i p) = sinQueso i (sinLactosa p)

-- recibe la pizza ya sin Queso
sinQueso :: Ingrediente -> Pizza -> Pizza
sinQueso Queso p = p
sinQueso i     p = Capa i p

-- Alternativa 2
-- sinLactosa :: Pizza -> Pizza
-- sinLactosa Prepizza   = Prepizza
-- sinLactosa (Capa i p) =
-- 	case i of
-- 		Queso -> sinLactosa p
-- 		i     -> Capa i (sinLactosa p)

aptaIntolerantesLactosa :: Pizza -> Bool
aptaIntolerantesLactosa Prepizza   = True 
aptaIntolerantesLactosa (Capa i p) = not (esQueso i) && (aptaIntolerantesLactosa p)

-- not
-- &&

-- tanto not como && tiene una tonelada de propiedades

-- como pasa con +, *, ...

esQueso :: Ingrediente -> Bool
esQueso Queso = True
esQueso _     = False

-- aptaIntolerantesLactosa :: Pizza -> Bool
-- aptaIntolerantesLactosa Prepizza   = True 
-- aptaIntolerantesLactosa (Capa i p) = if esQueso i
--                                         then False
--                                         else aptaIntolerantesLactosa p

-- Lo importante de las funciones recursivas es que cumplen con el esquema de recursión estructural
sinAceitunasConsecutivas :: Pizza -> Pizza
sinAceitunasConsecutivas Prepizza   = Prepizza 
sinAceitunasConsecutivas (Capa i p) = juntarAceitunas i (sinAceitunasConsecutivas p)

-- la pizza que recibe tiene no tiene aceitunas consecutivas
juntarAceitunas :: Ingrediente -> Pizza -> Pizza
juntarAceitunas (Aceitunas n) (Capa (Aceitunas m) p) = Capa (Aceitunas (n+m)) p
juntarAceitunas i             p                      = Capa i p

-- precondición: necesito al menos 3 elementos
-- sumarLosTresPrimerosElementos (x : y : z : xs) = x + y + z
-- sumarLosTresPrimerosElementos _ = error "no tiene al menos 3 elementos"

soloEsUnaCapaDeSalsa :: Pizza -> Bool
soloEsUnaCapaDeSalsa (Capa Salsa Prepizza) = True
soloEsUnaCapaDeSalsa _ = False

tieneSoloSalsa :: Pizza -> Bool
tieneSoloSalsa Prepizza   = True
tieneSoloSalsa (Capa i p) = esSalsa i && tieneSoloSalsa p

-- sinAceitunasConsecutivas (Capa (Aceitunas 3) (Capa (Aceitunas 4) (Capa Queso Prepizza))
-- ->
-- Capa (Aceitunas 7) (Capa Queso Prepizza)


-- sinAceitunasConsecutivas (Capa (Aceitunas 3) (Capa Queso (Capa (Aceitunas 4) Prepizza))
-- ->
-- (Capa (Aceitunas 3) (Capa Queso (Capa (Aceitunas 4) Prepizza))

-- sinAceitunasConsecutivas (Capa (Aceitunas 3) (Capa (Aceitunas 4) (Capa (Aceitunas 3) (Capa Salsa Prepizza)))
-- ->

-- Capa (Aceitunas 10) (Capa Salsa Prepizza)

-- cantidadDeAceitunas Prepizza = cantidadDeAceitunas (conDescripcionMejorada Prepizza)

-- -- lado izq
-- cantidadDeAceitunas Prepizza

-- -- lado der
-- cantidadDeAceitunas (sinAceitunasConsecutivas Prepizza)
-- = -- def sinAceitunasConsecutivas.1
-- cantidadDeAceitunas Prepizza

-- -- lado izq
-- cantidadDeAceitunas (Capa Queso Prepizza)

-- -- lado der
-- cantidadDeAceitunas (sinAceitunasConsecutivas (Capa Queso Prepizza))
-- = -- def sinAceitunasConsecutivas.2, i = Queso, p = Preppiza
-- cantidadDeAceitunas (juntarAceitunas Queso (sinAceitunasConsecutivas Preppiza))
-- = -- def sinAceitunasConsecutivas.1
-- cantidadDeAceitunas (juntarAceitunas Queso Prepizza)
-- = -- def juntarAceitunas.2
-- cantidadDeAceitunas (Capa Queso Prepizza)