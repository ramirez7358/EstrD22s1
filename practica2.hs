-- Funciones de practicas anteriores -- 
sucesor :: Int -> Int
sucesor n = n+1

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

maxDelPar :: (Int, Int) -> Int
maxDelPar (n, m) = if(n>m) then n
                   else         m

minDelPar :: (Int, Int) -> Int
minDelPar (n, m) = if(n>m) then m
                   else         n

edad :: Persona -> Int
edad (P _ e) = e

tipoDe :: Pokemon -> TipoDePokemon
tipoDe (PK tp _) = tp

unoSiEsTipo :: Pokemon -> TipoDePokemon -> Int
unoSiEsTipo p tp = if mismoTipo (tipoDe p) tp 
                    then 1
                    else 0

mismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
mismoTipo Fuego Fuego = True
mismoTipo Agua Agua = True
mismoTipo Planta Planta = True
mismoTipo _ _ = False

unoSi :: Bool -> Int
unoSi True = 1
unoSi _ = 0

tipoSuperiorA :: TipoDePokemon -> TipoDePokemon -> Bool
tipoSuperiorA Agua Fuego = True
tipoSuperiorA Fuego Planta = True
tipoSuperiorA Planta Agua = True
tipoSuperiorA _ _ = False

superaA :: Pokemon -> Pokemon -> Bool
superaA (PK t1 _) (PK t2 _) =  tipoSuperiorA t1 t2

-- Recursión sobre listas --

-- 1 --
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (n:ns) = n + sumatoria ns

-- 2 --
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- 3 --
sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (x:xs) = sucesor x : sucesores xs

-- 4 --
conjuncion :: [Bool] -> Bool
conjuncion [] = True 
conjuncion (b:bs) = and' b (conjuncion bs)

-- 5 --
disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (b:bs) = or' b (disyuncion bs)

-- 6 --
aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (l:ls) = l ++ aplanar ls

-- 7 --
pertenece :: Eq a => a -> [a] -> Bool
pertenece e [] = False
pertenece e (x:xs) = if x == e
                        then True
                        else pertenece e xs

-- 8 --
apariciones :: Eq a => a -> [a] -> Int
apariciones _ [] = 0
apariciones e (x:xs) = if x == e
                        then 1 + apariciones e xs
                        else apariciones e xs

-- 9 --
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA n [] = [] 
losMenoresA n (x:xs) = if x < n
                        then x : losMenoresA n xs
                        else losMenoresA n xs

-- 10 --
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA _ [] = [] 
lasDeLongitudMayorA n (l:ls) = if longitud l > n
                                then l : lasDeLongitudMayorA n ls
                                else lasDeLongitudMayorA n ls

-- 11 --
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] e = [e]
agregarAlFinal l e = l ++ [e]

-- Con recursion --
agregarAlFinal' :: [a] -> a -> [a]
agregarAlFinal' [] e = [e]
agregarAlFinal' (x:xs) e = x : agregarAlFinal xs e

-- 12 --
concatenar :: [a] -> [a] -> [a]
concatenar [] ys = ys
concatenar (x:xs) ys = x : concatenar xs ys

-- 13 --
reversa :: [a] -> [a]
reversa [] = [] 
reversa (x:xs) = agregarAlFinal (reversa xs) x

-- 14 --
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] _ = []
zipMaximos _ [] = []
zipMaximos (x:xs) (y:ys) = maxDelPar (x,y) : zipMaximos xs ys

-- 15 --
elMinimo :: Ord a => [a] -> a
elMinimo [x] = x
elMinimo (x:xs) = min x (elMinimo xs)

-- Recursión sobre números --

-- 1 --
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial(n - 1)

-- 2 --
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva n  = n : cuentaRegresiva(n - 1)

-- 3 --
repetir :: Int -> a -> [a]
repetir 0 _ = [] 
repetir n e = e : repetir (n - 1) e

-- 4 --
losPrimeros :: Int -> [a] -> [a]
losPrimeros 0 _ = []
losPrimeros _ [] = []
losPrimeros n (x:xs) = x :  losPrimeros (n-1) xs

-- 5 --
sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 0 xs = xs
sinLosPrimeros _ [] = []
sinLosPrimeros n (x:xs) = sinLosPrimeros (n - 1) xs

-- Registros --

data Persona = P String Int
    deriving Show

p1 = P "Brian" 26
p2 = P "Leonel" 25
p3 = P "Ramirez" 23
p4 = P "Brandan" 22
p5 = P "Roja" 21
p6 = P "Azul" 20

-- 1 --

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA _ [] = [] 
mayoresA 0 ps = ps
mayoresA n (p:ps) = if edad p >= n
                        then p : mayoresA n ps
                        else mayoresA n ps


sumatoriaDeEdades :: [Persona] -> Int
sumatoriaDeEdades [] = 0
sumatoriaDeEdades (p:ps) = edad p + sumatoriaDeEdades ps

promedioEdad :: [Persona] -> Int
promedioEdad ps = div (sumatoriaDeEdades ps) (longitud ps)

elMasViejoEntre :: Persona -> Persona -> Persona
elMasViejoEntre p1 p2 = if edad p1 > edad p2
                            then p1
                            else p2

elMasViejo :: [Persona] -> Persona
elMasViejo [p] = p 
elMasViejo (p:ps) = elMasViejoEntre p (elMasViejo ps)

-- 2 --

data TipoDePokemon = Agua | Fuego | Planta
data Pokemon = PK TipoDePokemon Int
data Entrenador = E String [Pokemon]

e1 = E "Brian" [(PK Fuego 100),(PK Fuego 100)]
e2 = E "Leonel" [(PK Agua 100),(PK Planta 100)]
e3 = E "Debora" [(PK Planta 100),(PK Planta 99)]

pokemon1 = PK Fuego 100
pokemon2 = PK Planta 100
pokemon3 = PK Agua 100
pokemon4 = PK Agua 90


cantPokemon :: Entrenador -> Int
cantPokemon (E _ pks) = longitud pks

pokemonDe :: TipoDePokemon -> [Pokemon] -> [Pokemon]
pokemonDe _ [] = []
pokemonDe tp (pk:pks) = if mismoTipo tp (tipoDe pk)
                            then pk : pokemonDe tp pks
                            else pokemonDe tp pks

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe tp (E n pks) = longitud (pokemonDe tp pks)

pokemonLeGanaATodos :: Pokemon -> [Pokemon] -> Bool
pokemonLeGanaATodos _ [] = True
pokemonLeGanaATodos p (pk:pks) = and' (superaA p pk) (pokemonLeGanaATodos p pks)

cantPokemonLeGanaATodos :: [Pokemon] -> [Pokemon] -> Int
cantPokemonLeGanaATodos [] _ = 0
cantPokemonLeGanaATodos (pk:pks) pksRival = unoSi (pokemonLeGanaATodos pk pksRival) + cantPokemonLeGanaATodos pks pksRival

losQueLeGanan :: TipoDePokemon -> Entrenador -> Entrenador -> Int
losQueLeGanan tp (E _ pks1) (E _ pks2) = cantPokemonLeGanaATodos (pokemonDe tp pks1) pks2