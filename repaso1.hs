-- Pizzas --

data Pizza = Prepizza | Capa Ingrediente Pizza
    deriving Show
data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int
    deriving Show

pizza0 = Capa Jamon (Capa Queso (Capa Salsa Prepizza))
pizza1 = Capa (Aceitunas 1) (Capa (Aceitunas 3) (Capa (Aceitunas 6) Prepizza ))

cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa i p) = 1 + cantidadDeCapas p

armarPizza :: [Ingrediente] -> Pizza
armarPizza [] = Prepizza
armarPizza (ig:igs) = Capa ig (armarPizza igs)

sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza = Prepizza
sacarJamon (Capa ig p) = if esJamon ig
                            then (sacarJamon p)
                            else (Capa ig (sacarJamon p))

esJamon :: Ingrediente -> Bool
esJamon Jamon = True
esJamon _ = False

esQueso :: Ingrediente -> Bool
esQueso Queso = True
esQueso _ = False

esSalsa :: Ingrediente -> Bool
esSalsa Salsa = True
esSalsa _ = False

mismoIngrediente :: Ingrediente -> Ingrediente -> Bool
mismoIngrediente Salsa Salsa = True
mismoIngrediente Queso Queso = True
mismoIngrediente Jamon Jamon = True
mismoIngrediente (Aceitunas _) (Aceitunas _) = True
mismoIngrediente _ _ = False

tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza = False
tieneSoloSalsaYQueso p = tieneIngrediente p Queso && tieneIngrediente p Salsa

tieneIngrediente :: Pizza -> Ingrediente -> Bool
tieneIngrediente Prepizza _ = False
tieneIngrediente (Capa ig p) ig2 = mismoIngrediente ig ig2 || tieneIngrediente p ig2

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa ig p) = (Capa (duplicarSiEsAceituna ig) (duplicarAceitunas p))

duplicarSiEsAceituna :: Ingrediente -> Ingrediente
duplicarSiEsAceituna (Aceitunas n) = (Aceitunas (n*2))
diplicarSiEsAceituna ig = ig

cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza [] = []
cantCapasPorPizza (pz:pzs) = (cantidadDeCapas pz,pz) : cantCapasPorPizza pzs

-- Mapa de tesoro --

data Dir = Izq | Der
    deriving Show
data Objeto = Tesoro | Chatarra
    deriving Show
data Cofre = Cofre [Objeto]
    deriving Show
data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa
    deriving Show

cofre0 = Cofre [Chatarra, Chatarra]
cofre1 = Cofre [Chatarra, Tesoro, Chatarra]

mapa0 = Bifurcacion cofre0 (Fin cofre1) (Fin cofre0)
mapa1 = Bifurcacion cofre0 (Fin cofre1) (Fin cofre1)
mapa2 = Bifurcacion cofre0 mapa0 mapa1
mapa3 = Bifurcacion cofre0 (Fin cofre0) (Fin cofre0)

-- Mapa con tesoro unico
mapa4 = Bifurcacion cofre0 (Bifurcacion cofre0 mapa3 mapa3) (Bifurcacion cofre0 mapa0 mapa3)
mapa5 = Bifurcacion cofre1 (Fin cofre0) (Fin cofre0)
mapa6 = Bifurcacion cofre0 mapa4 mapa4

hayTesoro :: Mapa -> Bool
hayTesoro (Fin c) = hayTesoroEnCofre c
hayTesoro (Bifurcacion c m1 m2) = hayTesoroEnCofre c || hayTesoro m1 || hayTesoro m2

hayTesoroEnCofre :: Cofre -> Bool
hayTesoroEnCofre c = hayTesoroEnLista (objetosDe c)

hayTesoroEnLista :: [Objeto] -> Bool
hayTesoroEnLista [] = False
hayTesoroEnLista (obj:objs) = esTesoro obj || hayTesoroEnLista objs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

objetosDe :: Cofre -> [Objeto]
objetosDe (Cofre objs) = objs

hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn [] m = hayTesoroEnCofre (cofre m)
hayTesoroEn ds m = hayTesoroEnCofre (cofre (avanzar ds m))

avanzar :: [Dir] -> Mapa -> Mapa
avanzar [] m = m
avanzar (d:ds) m = avanzar ds (avanzarHacia m d)

avanzarHacia :: Mapa -> Dir -> Mapa
avanzarHacia (Bifurcacion _ mi _) Izq = mi
avanzarHacia (Bifurcacion _ _ md) Der = md

abrirCofre :: Cofre -> [Objeto]
abrirCofre (Cofre objs) = objs

cofre :: Mapa -> Cofre
cofre (Fin c) = c
cofre (Bifurcacion c _ _) = c

caminoAlTesoro :: Mapa -> [Dir]
caminoAlTesoro (Fin _) = []
caminoAlTesoro m = if hayTesoroEnCofre (cofre m)
                    then []
                    else caminoAlTesoro' m

caminoAlTesoro' :: Mapa -> [Dir]
caminoAlTesoro' (Fin _) = []
caminoAlTesoro' (Bifurcacion _ m1 m2) = if hayTesoro m1
                                            then Izq : caminoAlTesoro' m1
                                            else Der : caminoAlTesoro' m2

pasosAlFin :: Mapa -> Int
pasosAlFin (Fin _) = 0
pasosAlFin (Bifurcacion _ m1 m2) = 1 + max (pasosAlFin m1) (pasosAlFin m2)

caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga (Fin _) = []
caminoDeLaRamaMasLarga (Bifurcacion _ m1 m2) = if pasosAlFin m1 > pasosAlFin m2
                                                then Izq : caminoDeLaRamaMasLarga m1
                                                else Der : caminoDeLaRamaMasLarga m2

singularSi :: a -> Bool -> [a]
singularSi x True = x:[]
singularSi x False = []

soloLosTesorosDe :: [Objeto] -> [Objeto]
soloLosTesorosDe [] = []
soloLosTesorosDe (obj:objs) = singularSi obj (esTesoro obj) ++ soloLosTesorosDe objs

-- Juntar por niveles --
tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel (Fin c) = [soloLosTesorosDe (abrirCofre c)]
tesorosPorNivel (Bifurcacion c m1 m2) = soloLosTesorosDe (abrirCofre c) : (tesorosPorNivel m1 ++ tesorosPorNivel m2)