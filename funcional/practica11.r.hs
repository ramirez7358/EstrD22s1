type Nombre = String
data Planilla = Fin Int | Registro Nombre Planilla

foldPlanilla :: (Int -> b) -> (Nombre -> b -> b) -> Planilla -> b
foldPlanilla fb _ (Fin n) = fb n
foldPlanilla cte f (Registro n r) = f n (foldPlanilla cte f r)

data Dungeon a = Habitacion a | PasajeCon a (Dungeon a) | PasajeSin (Dungeon a)

data Ingrediente = Aceituna Int | Queso | Jamon | Salsa deriving Show
data Pizza = Prepizza | Capa Ingrediente Pizza deriving Show

-- clase teorica

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x : xs) = f x (foldr' f z xs)

sumSqr :: [Int] -> Int
sumSqr = foldr' (\x xs -> x ^ 2 + xs) 0

subset :: Eq a => [a] -> [a] -> Bool
subset = foldr' (\ x h ys -> elem x ys && h ys) (\ys -> True)

-- 1

cantidadCapasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> Int
cantidadCapasQueCumplen _ Prepizza = 0
cantidadCapasQueCumplen p (Capa i pz) = let rr = cantidadCapasQueCumplen p pz
                                            in if p i
                                                then 1 + rr
                                                else rr

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

sinLactosa = soloLasCapasQue (not . esQueso)
aptaIntolerantesLactosa = (==0) . cantidadCapasQueCumplen esQueso
cantidadDeQueso = cantidadCapasQueCumplen esQueso
conElDobleDeAceitunas = conCapasTransformadas duplicarAceitunas

duplicarAceitunas (Aceituna n) = Aceituna (n + n)
duplicarAceitunas i = i

esQueso Queso = True
esQueso _ = False

-- 3

pizzaProcesada :: (Ingrediente -> b -> b) -> b -> Pizza -> b
pizzaProcesada fr fb Prepizza = fb
pizzaProcesada fr fb (Capa i p) = fr i (pizzaProcesada fr fb p)

-- 4

-- cantidadCapasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> Int
cantidadCapasQueCumplen' f = pizzaProcesada (\i n -> if f i then 1 + n else n) 0

-- soloLasCapasQue :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue' f = pizzaProcesada (\i p -> if f i then Capa i p else p) Prepizza

sinLactosa' = pizzaProcesada (\i p -> if esQueso i then p else Capa i p) Prepizza

aptaIntolerantesLactosa' = pizzaProcesada (\i p -> (not . esQueso) i && p) True

cantidadDeQueso' = pizzaProcesada (\i n -> if esQueso i then 1 + n else n) 0

conElDobleDeAceitunas' = pizzaProcesada (Capa . duplicarAceitunas) Prepizza

-- 5

cantidadAceitunas = pizzaProcesada ((+) . cantAceitunas) 0

cantAceitunas (Aceituna n) = n
cantAceitunas _ = 0

p1 = Capa Queso (Capa (Aceituna 3) (Capa (Aceituna 9) Prepizza))