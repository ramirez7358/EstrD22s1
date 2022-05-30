-- Tipos recursivos simples --
-- Celdas con bolitas --

data Color = Azul | Rojo
    deriving (Show)

data Celda = Bolita Color Celda | CeldaVacia
    deriving Show

celda0 = CeldaVacia
celda1 = Bolita Rojo CeldaVacia
celda2 = Bolita Rojo (Bolita Azul CeldaVacia)
celda3 = Bolita Rojo (Bolita Rojo CeldaVacia)

nroBolitas :: Color -> Celda -> Int
nroBolitas _ CeldaVacia      = 0
nroBolitas c (Bolita cb cel) = unoSi (mismoColor c cb) + nroBolitas c cel

mismoColor :: Color -> Color -> Bool
mismoColor Rojo Rojo = True
mismoColor Azul Azul = True
mismoColor _ _ = False

unoSi :: Bool -> Int
unoSi True  = 1
unoSi False = 0

singularSi :: a -> Bool -> [a]
singularSi x True = x:[]
singularSi x False = []

poner :: Color -> Celda -> Celda
poner c cl = Bolita c cl
{-
    Solucion con recursion
    poner c (Bolita cb cel) = (Bolita cb (poner c cel))
-}

sacar :: Color -> Celda -> Celda
sacar _ CeldaVacia = CeldaVacia
sacar c (Bolita cb cel) = if mismoColor cb c
                            then cel
                            else (Bolita cb (sacar c cel))

ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 _ cel = cel
ponerN n c cl = Bolita c (ponerN (n-1) c cl) 

-- Camino hacia el tesoro --

data Objeto = Cachorro | Tesoro
    deriving Show

data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
    deriving Show

camino0 = Fin
camino1 = Nada camino0
camino2 = Cofre [Cachorro,Cachorro] Fin
camino3 = Cofre [Tesoro,Cachorro] camino2
camino4 = Cofre [Cachorro] (Cofre [Cachorro,Cachorro] camino3)
camino5 = Cofre [Tesoro, Tesoro, Tesoro] camino4

hayTesoro :: Camino -> Bool
hayTesoro Fin = False
hayTesoro (Nada c) = hayTesoro c
hayTesoro (Cofre objs c) = hayTesoroEnLista objs || hayTesoro c

hayTesoroEnLista :: [Objeto] -> Bool
hayTesoroEnLista [] = False
hayTesoroEnLista (x:xs) = esTesoro x || hayTesoroEnLista xs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro Fin =  0
pasosHastaTesoro (Nada c) = 1 + pasosHastaTesoro c
pasosHastaTesoro (Cofre objs c) = if hayTesoroEnLista objs
                                    then 0
                                    else 1 + pasosHastaTesoro c

hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn p c = hayTesoroEnCamino (avanzarHasta p c)
{-hayTesoroEn _ Fin            = False
hayTesoroEn n (Nada c)       = hayTesoroEn (n-1) c
hayTesoroEn n (Cofre objs c) = if n == 0
                                then hayTesoroEnLista objs
                                else hayTesoroEn (n-1) c-}

hayTesoroEnCamino :: Camino -> Bool
hayTesoroEnCamino (Cofre objs _) = hayTesoroEnLista objs
hayTesoroEnCamino _ = False

cantidadDeTesoros :: [Objeto] -> Int
cantidadDeTesoros [] = 0
cantidadDeTesoros (obj:objs) = unoSi (esTesoro obj) + cantidadDeTesoros objs

alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros 0 _ = True
alMenosNTesoros _ Fin = False
alMenosNTesoros n (Nada c) = alMenosNTesoros n c
alMenosNTesoros n (Cofre objs c) = let ct = cantidadDeTesoros objs in ct >= n ||  alMenosNTesoros (n - ct) c
--alMenosNTesoros n (Cofre objs c) = cantidadDeTesoros objs >= n || alMenosNTesoros (n - cantidadDeTesoros objs) c

cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre d h c = cantidadDeTesorosHasta (h -d) (avanzarHasta d c)

cantidadDeTesorosHasta :: Int -> Camino -> Int
cantidadDeTesorosHasta n Fin = 0
cantidadDeTesorosHasta n (Nada c) = cantidadDeTesorosHasta (n - 1) c
cantidadDeTesorosHasta n (Cofre objs c) = cantidadDeTesoros objs + cantidadDeTesorosHasta (n-1) c

avanzarHasta :: Int -> Camino -> Camino
avanzarHasta 0 c = c
avanzarHasta _ Fin = Fin
avanzarHasta n (Nada c) = avanzarHasta (n-1) c
avanzarHasta n (Cofre _ c) = avanzarHasta (n-1) c

-- Tipos arboreos --
-- Arboles binarios --

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) 
    deriving Show

tr0 = EmptyT
tr1' = NodeT 77 (EmptyT) (EmptyT)
tr1 = NodeT 9 (EmptyT) (EmptyT)
tr2 = NodeT 7 (tr1) (tr0)
tr3 = NodeT 10 tr2 tr1'
tr4 = NodeT 20 EmptyT tr3
tr5 = NodeT 55 tr4 tr3
tr6 = NodeT 99 (EmptyT) tr2
tr7 = NodeT 1 tr1 tr1'

sumarT :: Tree Integer -> Integer
sumarT EmptyT = 0
sumarT (NodeT n t1 t2) = n + (sumarT t1) + (sumarT t2)

sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT _ t1 t2) = 1 + sizeT t1 + sizeT t2

mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT e t1 t2) = (NodeT (e*2) (mapDobleT t1) (mapDobleT t2))

perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT _ EmptyT = False
perteneceT e (NodeT en t1 t2) = e == en || perteneceT e t1 || perteneceT e t2

aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT _ EmptyT = 0
aparicionesT e (NodeT ne t1 t2) = unoSi (e == ne) + aparicionesT e t1 + aparicionesT e t2

leaves :: Tree a -> [a]
leaves EmptyT = [] 
leaves (NodeT e t1 t2) = singularSi e (esEmpty t1 && esEmpty t2) ++ leaves t1 ++ leaves t2

esNodoSinRamas :: Tree a -> Bool
esNodoSinRamas EmptyT = True
esNodoSinRamas (NodeT e EmptyT EmptyT) = True
esNodoSinRamas _ = False

esEmpty :: Tree a -> Bool
esEmpty EmptyT = True
esEmpty _ = False

heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT en t1 t2) = 1 + max (heightT t1) (heightT t2)

mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT
mirrorT (NodeT en t1 t2) = (NodeT en (mirrorT t2) (mirrorT t1))

toList :: Tree a -> [a]
toList EmptyT = []
toList (NodeT en t1 t2) = toList t1 ++ en:[] ++ toList t2

levelN :: Int -> Tree a -> [a]
levelN _ EmptyT = []
levelN 0 (NodeT en t1 t2) = en:[]
levelN n (NodeT en t1 t2) = levelN (n-1) t1 ++ levelN (n-1) t2

listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []
listPerLevel (NodeT en t1 t2) = [en] : juntarNivel (listPerLevel t1) (listPerLevel t2)

juntarNivel :: [[a]] -> [[a]] -> [[a]]
juntarNivel []       yss      = yss
juntarNivel xss        []     = xss
juntarNivel (xs:xss) (ys:yss) = (xs ++ ys) : juntarNivel xss yss

ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT en t1 t2) = en : listaMasLarga (ramaMasLarga t1) (ramaMasLarga t2)
--ramaMasLarga (NodeT en t1 t2) = en:[] ++ ramaMasLarga (nodoMasAlto t1 t2)

listaMasLarga :: [a] -> [a] -> [a]
listaMasLarga n1 n2 = if length n1 > length n2
                        then n1
                        else n2

nodoMasAlto :: Tree a -> Tree a -> Tree a
nodoMasAlto t1 t2 = if (heightT t1 > heightT t2) then t1 else t2

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT e t1 t2) = (e:[]) : agregar e (todosLosCaminos t1) ++ agregar e (todosLosCaminos t2)

agregar :: a -> [[a]] -> [[a]]
agregar x []  = []
agregar x (ys:yss) = (x:ys) : agregar x yss 

data ExpA = Valor Int 
          | Sum ExpA ExpA
          | Prod ExpA ExpA
          | Neg ExpA
    deriving Show

eval :: ExpA -> Int
eval (Valor n) = n
eval (Sum exp1 exp2) = eval exp1 + eval exp2
eval (Prod exp1 exp2) = eval exp1 * eval exp2
eval (Neg exp) = -(eval exp)

simplificar :: ExpA -> ExpA
simplificar (Valor n) = (Valor n) 
simplificar (Sum x y) = simplificarSuma x y
simplificar (Prod x y) = simplificarProducto x y
simplificar (Neg x) = simplificarNegacion x

simplificarSuma :: ExpA -> ExpA -> ExpA
simplificarSuma (Valor 0) y = y
simplificarSuma x (Valor 0) = x
simplificarSuma x y = Sum x y

simplificarProducto :: ExpA -> ExpA -> ExpA
simplificarProducto (Valor 0) y = (Valor 0)
simplificarProducto x (Valor 0) = (Valor 0)
simplificarProducto (Valor 1) y = y
simplificarProducto x (Valor 1) = x
simplificarProducto x y = Prod x y

simplificarNegacion :: ExpA -> ExpA
simplificarNegacion (Neg x) = x
simplificarNegacion x = Neg x