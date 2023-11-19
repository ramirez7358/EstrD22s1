map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x : xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x : xs) =
  let r = filter' p xs
   in if p x
        then x : r
        else r

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x : xs) = f x (foldr' f z xs)

type Empleado = String

type Empresa = [Empleado]

ausencias :: Empleado -> Int
ausencias e = 1

losSarmiento :: Empresa -> [Empleado]
losSarmiento = filter' ((== 0) . ausencias)

presentismos :: [Empresa] -> [Empresa] -- Empresas pero sin personal sin ausencias
presentismos = filter' (null . losSarmiento)

-- Funciones con foldr

map'' f = foldr' (\e ep -> f e : ep) []

filter'' p = foldr' (\x -> if p x then (x :) else id) []

append'' = foldr' (\x h ys -> x : h ys) id

recr :: b -> (a -> [a] -> b -> b) -> [a] -> b
recr z f [] = z
recr z f (x : xs) = f x xs (recr z f xs)

prod' = foldr' (*) 1

-- Comienzo de la practica

data Pizza = Prepizza | Capa Ingrediente Pizza
  deriving (Show)

data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int
  deriving (Show)

-- Pizzas de prueba
pizza1 :: Pizza
pizza1 = Capa Queso (Capa Salsa (Capa Jamon Prepizza))

pizza2 :: Pizza
pizza2 = Capa (Aceitunas 5) (Capa Queso (Capa (Aceitunas 3) (Capa Salsa Prepizza)))

pizza3 :: Pizza
pizza3 = Prepizza

-- Predicados de prueba
esQueso :: Ingrediente -> Bool
esQueso Queso = True
esQueso _ = False

esAceituna :: Ingrediente -> Bool
esAceituna (Aceitunas _) = True
esAceituna _ = False

esSalsaOJamon :: Ingrediente -> Bool
esSalsaOJamon Salsa = True
esSalsaOJamon Jamon = True
esSalsaOJamon _ = False

-- 1

cantidadCapasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> Int
cantidadCapasQueCumplen _ Prepizza = 0
cantidadCapasQueCumplen p (Capa i pz) =
  let cr = cantidadCapasQueCumplen p pz
   in if p i
        then 1 + cr
        else cr

conCapasTransformadas :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza
conCapasTransformadas _ Prepizza = Prepizza
conCapasTransformadas f (Capa i pz) = Capa (f i) (conCapasTransformadas f pz)

soloLasCapasQue :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue _ Prepizza = Prepizza
soloLasCapasQue p (Capa i pz) =
  let cr = soloLasCapasQue p pz
   in if p i
        then Capa i cr
        else cr

-- 2

sinLactosa :: Pizza -> Pizza
sinLactosa = soloLasCapasQue (not . esQueso)

aptaIntolerantesLactosa :: Pizza -> Bool
aptaIntolerantesLactosa = (== 0) . cantidadCapasQueCumplen esQueso

cantidadDeQueso :: Pizza -> Int
cantidadDeQueso = cantidadCapasQueCumplen esQueso

conElDobleDeAceitunas :: Pizza -> Pizza
conElDobleDeAceitunas = conCapasTransformadas f
  where
    f (Aceitunas x) = Aceitunas (x * 2)
    f i = i

-- 3

pizzaProcesada :: (Ingrediente -> b -> b) -> b -> Pizza -> b
pizzaProcesada f z Prepizza = z
pizzaProcesada f z (Capa i pz) = f i (pizzaProcesada f z pz)

-- 4: Definir funciones de punto 1 y 2 con pizzaProcesada

cantidadCapasQueCumplen' :: (Ingrediente -> Bool) -> Pizza -> Int
cantidadCapasQueCumplen' predicate = pizzaProcesada (\i pp -> if predicate i then 1 + pp else pp) 0

conCapasTransformadas' :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza
conCapasTransformadas' tc = pizzaProcesada (Capa . tc) Prepizza

soloLasCapasQue' :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue' predicate = pizzaProcesada (\i pp -> if predicate i then Capa i pp else pp) Prepizza

sinLactosa' :: Pizza -> Pizza
sinLactosa' = pizzaProcesada (\i pp -> if esQueso i then pp else Capa i pp) Prepizza

aptaIntolerantesLactosa' :: Pizza -> Bool
-- aptaIntolerantesLactosa' = pizzaProcesada (\i pp -> not esQueso i && pp) True
aptaIntolerantesLactosa' = pizzaProcesada ((&&) . not . esQueso) True

cantidadDeQueso' :: Pizza -> Int
cantidadDeQueso' = pizzaProcesada (\i pp -> if esQueso i then 1 + pp else pp) 0

conElDobleDeAceitunas' :: Pizza -> Pizza
conElDobleDeAceitunas' = pizzaProcesada (\i pp -> if esAceituna i then Capa (duplicarAceitunas i) pp else pp) Prepizza
  where
    duplicarAceitunas (Aceitunas x) = Aceitunas (x * 2)
    duplicarAceitunas i = i

-- 5

cantidadAceitunas :: Pizza -> Int
-- cantidadAceitunas = pizzaProcesada (\i pp -> aceitunas i + pp) 0
--   where
--     aceitunas (Aceitunas x) = x
--     aceitunas _ = 0
cantidadAceitunas = pizzaProcesada ((+) . aceitunas) 0
  where
    aceitunas (Aceitunas x) = x
    aceitunas _ = 0

capasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> [Ingrediente]
capasQueCumplen predicate = pizzaProcesada (\i pp -> if predicate i then i : pp else pp) []

conDescripcionMejorada :: Pizza -> Pizza
conDescripcionMejorada = pizzaProcesada comprimirAceitunas Prepizza
  where
    comprimirAceitunas (Aceitunas n) (Capa (Aceitunas m) p) = Capa (Aceitunas (n + m)) p
    comprimirAceitunas i p = Capa i p

-- ¿Qué significa que le envie un tercer parametro a pizzaProcesada (foldPizza)?
-- Haciendo `(pizzaProcesada (\i pp -> Capa i pp)) pz` hago que la primera pizza sea el caso base.
-- Eso no me sirve por que el enunciado pide que los ingredientes de la pz1 se agreguen a la pz2
-- Hago un flip para que la expresion `(pizzaProcesada (\i pp -> Capa i pp))` pida como primer parametro
-- la pizza a recorrer.
conCapasDe :: Pizza -> Pizza -> Pizza
conCapasDe pz = flip (pizzaProcesada (\i pp -> Capa i pp)) pz

-- conCapasDe = flip (pizzaProcesada Capa) Version simplificada

-- pizzaProcesada :: (Ingrediente -> b -> b) -> b -> Pizza -> b
-- pizzaProcesada f z Prepizza = z
-- pizzaProcesada f z (Capa i pz) = f i (pizzaProcesada f z pz)

primerasNCapas :: Int -> Pizza -> Pizza
primerasNCapas 0 _ = Prepizza
primerasNCapas n Prepizza = Prepizza
primerasNCapas n (Capa i pz) = Capa i (primerasNCapas (n - 1) pz)

primerasNCapas' :: Int -> Pizza -> Pizza
primerasNCapas' n pz =
  pizzaProcesada
    g
    (\n -> Prepizza)
    pz
    n
  where
    g i pp 0 = Prepizza
    g i pp n = Capa i (pp (n - 1))

sacarNCapas :: Int -> Pizza -> Pizza
sacarNCapas n pz =
  pizzaProcesada
    g
    (const Prepizza)
    pz
    n
  where
    g i pp 0 = Capa i (pp (-1))
    g _ pp n = pp (n - 1)

-- 6

-- length . capasQueCumplen f = cantidadDe f