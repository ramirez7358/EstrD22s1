-- Funciones de practicas anteriores -- 
sucesor :: Int -> Int
sucesor n = n+1

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
conjuncion (b:bs) = b && (conjuncion bs)

-- 5 --
disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (b:bs) = b || (disyuncion bs)

-- 6 --
aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (l:ls) = l ++ aplanar ls

-- 7 --
pertenece :: Eq a => a -> [a] -> Bool
pertenece e [] = False
pertenece e (x:xs) = x == e || pertenece e xs 

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
agregarAlFinal (x:xs) e = x : agregarAlFinal xs e

-- 12 --
agregar :: [a] -> [a] -> [a]
agregar [] ys = ys
agregar (x:xs) ys = x : agregar xs ys

-- 13 --
reversa :: [a] -> [a]
reversa [] = [] 
reversa (x:xs) = agregarAlFinal (reversa xs) x

-- 14 --
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos xs [] = xs
zipMaximos [] ys = ys
zipMaximos (x:xs) (y:ys) = max x y : (zipMaximos xs ys)

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
cuentaRegresiva n  = if n <= 0
                        then []
                        else n : cuentaRegresiva(n - 1)

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
e4 = E "Doña Rosa" [(PK Planta 100),(PK Fuego 100),(PK Agua 100)]

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
pokemonLeGanaATodos p (pk:pks) = (superaA p pk) && (pokemonLeGanaATodos p pks)

cantPokemonLeGanaATodos :: [Pokemon] -> [Pokemon] -> Int
cantPokemonLeGanaATodos [] _ = 0
cantPokemonLeGanaATodos (pk:pks) pksRival = unoSi (pokemonLeGanaATodos pk pksRival) + cantPokemonLeGanaATodos pks pksRival

losQueLeGanan :: TipoDePokemon -> Entrenador -> Entrenador -> Int
losQueLeGanan tp (E _ pks1) (E _ pks2) = cantPokemonLeGanaATodos (pokemonDe tp pks1) pks2

tienePokemonDe :: [Pokemon] -> TipoDePokemon -> Bool
tienePokemonDe [] tp = False
tienePokemonDe (pk:pks) tp = mismoTipo tp (tipoDe pk) || tienePokemonDe pks tp

hayTodosLosTipos :: [Pokemon] -> Bool
hayTodosLosTipos pks = tienePokemonDe pks Fuego && tienePokemonDe pks Agua && tienePokemonDe pks Planta

esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (E _ pks) = hayTodosLosTipos pks


-- 3 --

data Seniority = Junior | SemiSenior | Senior
data Proyecto = ConsProyecto String
    deriving (Show)
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
data Empresa = ConsEmpresa [Rol]

empresa1 = ConsEmpresa [rol1, rol2, rol3, rol4, rol5]
empresa2 = ConsEmpresa [rol1, rol2, rol3, rol4, rol5, rol6, rol7, rol8, rol9, rol10, rol11, rol12, rol13, rol14, rol15, rol16, rol17, rol18, rol19, rol20]

rol1  = (Developer Senior pro1)
rol2 = (Management Senior pro1)
rol3 = (Management SemiSenior pro2) 
rol4 = (Developer Senior pro2)
rol5 = (Developer Senior pro3)
rol6 = (Developer Senior pro3)
rol7 = (Developer Senior pro3)
rol8 = (Developer Senior pro4)
rol9 = (Developer Senior pro5)
rol10 = (Developer Senior pro6)
rol11 = (Developer Senior pro4)
rol12 = (Developer Senior pro6)
rol13 = (Developer Senior pro6)
rol14 = (Developer Senior pro6)
rol15 = (Developer Senior pro6)
rol16 = (Developer Senior pro6)
rol17 = (Developer Senior pro6)
rol18 = (Developer Senior pro6)
rol19 = (Developer Senior pro6)
rol20 = (Developer Senior pro6)

pro1 = ConsProyecto "facebook"
pro2 = ConsProyecto "twitter"
pro3 = ConsProyecto "instagram"
pro4 = ConsProyecto "telegram"
pro5 = ConsProyecto "whatsapp"
pro6 = ConsProyecto "sube"


proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa rs) = proyectosSinRepetidos (proyectosDeRoles rs)

proyectosDeRoles :: [Rol] -> [Proyecto]
proyectosDeRoles [] = []
proyectosDeRoles (rl:rls) = proyecto rl : proyectosDeRoles rls

proyecto :: Rol -> Proyecto
proyecto (Developer _ p) = p
proyecto (Management _ p) = p

proyectosSinRepetidos :: [Proyecto] -> [Proyecto]
proyectosSinRepetidos [] = []
proyectosSinRepetidos (p:ps) = agregarProyectoSiNoEsta p (proyectosSinRepetidos ps)

agregarProyectoSiNoEsta :: Proyecto -> [Proyecto] -> [Proyecto]
agregarProyectoSiNoEsta p ps = if proyectoEn p ps
                                then ps
                                else p:ps

proyectoEn :: Proyecto -> [Proyecto] -> Bool
proyectoEn pb [] = False
proyectoEn pb (p:ps) = (nombre pb) == (nombre p) ||  proyectoEn pb ps

nombre :: Proyecto -> String
nombre (ConsProyecto n) = n

losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior e [] = 0
losDevSenior (ConsEmpresa rs) ps = longitud (losQueTrabajanEn (devsSenior rs) ps)

devsSenior :: [Rol] -> [Rol]
devsSenior [] = []
devsSenior (r:rs) = if esDevSenior r
                        then r : devsSenior rs
                        else devsSenior rs

esDevSenior :: Rol -> Bool
esDevSenior (Developer Senior _) = True
esDevSenior _                    = False


cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn ps (ConsEmpresa rs) = longitud (losQueTrabajanEn rs ps)

losQueTrabajanEn :: [Rol] -> [Proyecto] -> [Rol]
losQueTrabajanEn []     _  = []
losQueTrabajanEn (r:rs) ps = if trabajaEn r ps 
    	                        then r : losQueTrabajanEn rs ps
    	                        else losQueTrabajanEn rs ps

trabajaEn :: Rol -> [Proyecto] -> Bool
trabajaEn _ []       = False
trabajaEn r (p:ps) = (nombre (proyecto r)) == (nombre p) || trabajaEn r ps

asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto (ConsEmpresa rs) = asignadosPorProyectoR rs

asignadosPorProyectoR :: [Rol] -> [(Proyecto, Int)]
asignadosPorProyectoR [] = []
asignadosPorProyectoR (r:rs) = computarProyectosDe r (asignadosPorProyectoR rs)

computarProyectosDe :: Rol -> [(Proyecto, Int)] -> [(Proyecto, Int)]
computarProyectosDe r [] = [(proyecto r, 1)]
computarProyectosDe r (tpl:tpls) = if mismoProyecto (proyecto r) (fst tpl)
                                    then (fst tpl, snd tpl + 1) : tpls
                                    else tpl : computarProyectosDe r tpls

mismoProyecto :: Proyecto -> Proyecto -> Bool
mismoProyecto p1 p2 = nombre p1 == nombre p2


-- Solucion mal --
{-asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto e = cantidadRolesPorProyecto (proyectos e) (roles e) 
 
cantidadRolesPorProyecto :: [Proyecto] -> [Rol] -> [(Proyecto, Int)]
cantidadRolesPorProyecto [] _  = []
cantidadRolesPorProyecto (p:ps) rs = (p, cantDeRolesEn rs p) : cantidadRolesPorProyecto ps rs

cantDeRolesEn :: [Rol] -> Proyecto -> Int
cantDeRolesEn [] _ = 0
cantDeRolesEn (r:rs) p = unoSi ( trabajaEn r [p] ) + cantDeRolesEn rs p-}

roles :: Empresa -> [Rol]
roles (ConsEmpresa rs) = rs