data Color = Azul | Rojo
    deriving (Show,Eq)

data Celda = Bolita Color Celda | CeldaVacia
    deriving Show

data Objeto = Cachorro | Tesoro
    deriving (Show, Eq)

data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
    deriving Show

celda0 = CeldaVacia
celda1 = Bolita Rojo CeldaVacia
celda2 = Bolita Rojo (Bolita Azul CeldaVacia)
celda3 = Bolita Rojo (Bolita Rojo CeldaVacia)

camino0 = Fin
camino1 = Nada camino0
camino2 = Cofre [Cachorro,Cachorro] camino0
camino3 = Cofre [Tesoro,Cachorro] camino2
camino4 = Cofre [Cachorro] (Cofre [Cachorro,Cachorro] camino3)
camino5 = Cofre [Tesoro] Fin


nroBolitas :: Color -> Celda -> Int
nroBolitas _ CeldaVacia      = 0
nroBolitas c (Bolita cb cel) = unoSi (c == cb) + nroBolitas c cel

unoSi :: Bool -> Int
unoSi True  = 1
unoSi False = 0

singularSi :: a -> Bool -> [a]
singularSi x True = x:[]
singularSi x False = []

poner :: Color -> Celda -> Celda
poner c CeldaVacia = (Bolita c CeldaVacia)
poner c (Bolita cb cel) = (Bolita cb (poner c cel))

sacar :: Color -> Celda -> Celda
sacar _ CeldaVacia = CeldaVacia
sacar c (Bolita cb cel) = if cb == c
                            then cel
                            else (Bolita cb (sacar c cel))

ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 _ cel = cel
ponerN n c CeldaVacia = Bolita c (ponerN (n-1) c CeldaVacia)
ponerN n c (Bolita cb cel) = Bolita cb (ponerN (n-1) c cel)


hayTesoro :: Camino -> Bool
hayTesoro Fin = False
hayTesoro (Nada c) = hayTesoro c
hayTesoro (Cofre objs c) = hayTesoroEnLista objs || hayTesoro c

hayTesoroEnLista :: [Objeto] -> Bool
hayTesoroEnLista [] = False
hayTesoroEnLista (x:xs) = x == Tesoro || hayTesoroEnLista xs

pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro Fin =  0
pasosHastaTesoro (Nada c) = 1 + pasosHastaTesoro c
pasosHastaTesoro (Cofre objs c) = if hayTesoroEnLista objs
                                    then 0
                                    else 1 + pasosHastaTesoro c

hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn _ Fin            = False
hayTesoroEn n (Nada c)       = hayTesoroEn (n-1) c
hayTesoroEn 0 (Cofre objs c) = hayTesoroEnLista objs
hayTesoroEn n (Cofre objs c) = hayTesoroEn (n-1) c


-- Esta mal, no se como hacerlo
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n Fin = if n == 0 then True else False
alMenosNTesoros n (Nada c) = alMenosNTesoros n c
alMenosNTesoros n (Cofre objs c) = alMenosNTesoros (n-1) c && hayTesoroEnLista objs


data Tree a = EmptyT | NodeT a (Tree a) (Tree a) 
    deriving Show

tr0 = EmptyT
tr1 = NodeT 1 (EmptyT) (EmptyT)
tr2 = NodeT 7 (tr1) (tr0)
tr3 = NodeT 10 tr2 tr1
tr4 = NodeT 20 EmptyT tr3

sumarT :: Tree Integer -> Integer
sumarT EmptyT = 0
sumarT (NodeT n t1 t2) = n + (sumarT t1) + (sumarT t2)

sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT e t1 t2) = 1 + sizeT t1 + sizeT t2

mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT e t1 t2) = (NodeT (e*2) (mapDobleT t1) (mapDobleT t2))

perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT _ EmptyT = False
perteneceT e (NodeT en t1 t2) = e == en || perteneceT e t1 || perteneceT e t2

aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT _ EmptyT = 0
aparicionesT e (NodeT en t1 t2) = unoSi (e == en) + (aparicionesT e t1) + (aparicionesT e t2)

leaves :: Tree a -> [a]
leaves EmptyT = []
leaves (NodeT en t1 t2) = en:[] ++ (leaves t1) ++ (leaves t2)

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
levelN n EmptyT = []
levelN 0 (NodeT en t1 t2) = en:[]
levelN n (NodeT en t1 t2) = levelN (n-1) t1 ++ levelN (n-1) t1 

-- Esta mal, revisar
listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []
listPerLevel (NodeT en t1 t2) = [en] : (listPerLevel t1 ++ listPerLevel t2)

ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT en t1 t2) = en:[] ++ ramaMasLarga (nodoMasAlto t1 t2)

nodoMasAlto :: Tree a -> Tree a -> Tree a
nodoMasAlto t1 t2 = if (heightT t1 > heightT t2) then t1 else t2

--todosLosCaminos :: Tree a -> [[a]]
--todosLosCaminos EmptyT = ...
--todosLosCaminos (NodeT en t1 t2)

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
simplificar (Sum exp1 exp2) = if (eval (simplificar exp1) == 0) then exp2 else exp1