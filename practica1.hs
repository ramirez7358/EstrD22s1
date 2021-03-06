-- Números enteros --
-- 1 --

-- a --
sucesor :: Int -> Int
sucesor n = n+1

-- b --
sumar :: Int -> Int -> Int
sumar n m = n + m

-- c --
divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto n m = (div n m , mod n m)

-- d --
maxDelPar :: (Int, Int) -> Int
maxDelPar (n, m) = if(n>m) then n
                   else         m

-- maxDelPar (sumar 5 (maxDelPar (divisionYResto 50 10)), sucesor 9)
-- sumar (-10) (sucesor (maxDelPar (divisionYResto 190 10)))
-- sucesor (sumar (maxDelPar (divisionYResto 8 2)) (maxDelPar (divisionYResto 10 2)))
-- sumar (sumar 3 2) (sucesor (sucesor (maxDelPar (divisionYResto 9 3))))

-- Tipos enumerativos --
-- 1 --

data Dir = Norte | Este | Sur | Oeste
     deriving Eq

equals :: Eq a => a -> a -> Bool
equals x y = x == y

-- a --
opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Este = Oeste
opuesto Sur = Norte
opuesto Oeste = Este

-- b --
iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Oeste Oeste = True
iguales Este Este = True
iguales Sur Sur = True
iguales _ _ = False

iguales2 :: Dir -> Dir -> Bool
iguales2 a b = equals a b

-- c --
siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste
siguiente Oeste = Norte

-- 2 --

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo

-- a --
primerYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primerYUltimoDia = (Lunes,Domingo)

-- b --
empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM _ = False

-- c --
numeroDeDia :: DiaDeSemana -> Int
numeroDeDia Lunes = 1
numeroDeDia Martes = 2
numeroDeDia Miercoles = 3
numeroDeDia Jueves = 4
numeroDeDia Viernes = 5
numeroDeDia Sabado = 6
numeroDeDia Domingo = 7

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues d1 d2 = numeroDeDia d1 > numeroDeDia d2

-- d --
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes = False
estaEnElMedio Domingo = False
estaEnElMedio _ = True

-- 3 --

-- a --
negar :: Bool -> Bool
negar True = False
negar False = True

-- b --
implica :: Bool -> Bool -> Bool
implica True False  = True
implica _ _ = False

-- c --
and :: Bool -> Bool -> Bool
and True b = b
and False _ = False

-- d --
or :: Bool -> Bool -> Bool
or False b = b
or True _ = True

-- Registros --
-- 1 --

data Persona = P String Int
     deriving Show

nombre :: Persona -> String
nombre (P n _) = n

edad :: Persona -> Int
edad (P _ e) = e

crecer :: Persona -> Persona
crecer (P n e) = (P n (e+1))

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre n (P _ e) = (P n e)

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra (P _ e1) (P _ e2) = e1 > e2

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 = if (edad p1 > edad p2) then p1
                                     else        p2

-- 2 --

data TipoDePokemon = Agua | Fuego | Planta
     deriving (Show, Eq)

data Pokemon = PK TipoDePokemon Int
     deriving Show

data Entrenador = E String Pokemon Pokemon
     deriving Show

entrenador1 = E "Brian" (PK Agua 100) (PK Fuego 100)
entrenador2 = E "Leonel" (PK Agua 100) (PK Planta 100)
entrenador3 = E "Debora" (PK Planta 100) (PK Planta 99)

pokemonesDe :: Entrenador -> [Pokemon]
pokemonesDe (E _ pk1 pk2) = pk1 : pk2 : []

tipoDe :: Pokemon -> TipoDePokemon
tipoDe (PK tp _) = tp

esTipo :: Pokemon -> TipoDePokemon -> Bool
esTipo pk tp = mismoTipo (tipoDe pk) tp

tipoSuperiorA :: TipoDePokemon -> TipoDePokemon -> Bool
tipoSuperiorA Agua Fuego = True
tipoSuperiorA Fuego Planta = True
tipoSuperiorA Planta Agua = True
tipoSuperiorA _ _ = False

superaA :: Pokemon -> Pokemon -> Bool
superaA (PK t1 _) (PK t2 _) =  tipoSuperiorA t1 t2

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe tp (E _ pk1 pk2) = unoSi (esTipo pk1 tp) + unoSi (esTipo pk2 tp)

unoSi :: Bool -> Int
unoSi True = 1
unoSi False = 0

mismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
mismoTipo Fuego Fuego = True
mismoTipo Agua Agua = True
mismoTipo Planta Planta = True
mismoTipo _ _ = False

juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon entrenadores = pokemonesDe (fst entrenadores) ++ pokemonesDe (snd entrenadores)

-- Funciones polimorficas --
-- 5 --

loMismo :: a -> a
loMismo a = a

siempreSiete :: a -> Int
siempreSiete a = 7

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

-- Pattern Matching sobre listas --
-- 5 --

estaVacia :: [a] -> Bool
estaVacia (_:_) = False
estaVacia _ = True

elPrimero :: [a] -> a
elPrimero (a:_) = a

sinElPrimero :: [a] -> [a]
sinElPrimero (_:xs) = xs

splitHead :: [a] -> (a, [a])
splitHead (x:xs) = (x,xs)