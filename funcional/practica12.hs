-- Ejemplo de recurción mutua

data GTree a = GNode a [GTree a]

sumGTPO :: GTree Int -> Int
sumGTPO (GNode x ts) = x + aRaTyS ts

aRaTyS :: [GTree Int] -> Int
aRaTyS [] = 0
aRaTyS (t : ts) = sumGTPO t + aRaTyS ts

-- Inicio de la practica

data ExpA = Cte Int | Suma ExpA ExpA | Prod ExpA ExpA deriving (Show)

-- 1
-- a
foldExpA :: (Int -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExpA -> b
foldExpA f g h (Cte x) = f x
foldExpA f g h (Suma t1 t2) = g (foldExpA f g h t1) (foldExpA f g h t2)
foldExpA f g h (Prod t1 t2) = h (foldExpA f g h t1) (foldExpA f g h t2)

-- b
-- que describe la cantidad de ceros explícitos en la expresión dada.
cantidadDeCeros :: ExpA -> Int
cantidadDeCeros = foldExpA c s p
  where
    c n = if n == 0 then 1 else 0
    s r1 r2 = r1 + r2
    p r1 r2 = r1 + r2

-- que describe si la expresión dada no tiene números negativos de manera explícita
noTieneNegativosExplicitosExpA :: ExpA -> Bool
noTieneNegativosExplicitosExpA = foldExpA (>= 0) (&&) (&&)

-- where
--     c n = n >= 0
--     s r1 r2 = r1 && r2
--     p r1 r2 = r1 && r2

-- que describe una expresión con el mismo significado que la dada,
-- pero que no tiene sumas del número 0 ni multiplicaciones por 1 o por 0.
-- La resolución debe ser exclusivamente simbólica.
simplificarExpA' :: ExpA -> ExpA
simplificarExpA' = foldExpA c s p
  where
    c = Cte
    s r1 r2 = case (esCero r1, esCero r2) of
      (True, _) -> r2
      (_, True) -> r1
      _ -> Suma r1 r2
    p r1 r2
      | esCero r1 || esCero r2 = Cte 0
      | esUno r1 = r2
      | esUno r2 = r1
      | otherwise = Prod r1 r2

-- que describe el número que resulta de evaluar la cuenta representada por la expresión aritmética dada.
evalExpA :: ExpA -> Int
evalExpA = foldExpA c s p
  where
    c n = n
    s r1 r2 = r1 + r2
    p r1 r2 = r1 * r2

-- que describe el string sin espacios y con paréntesis correspondiente a la expresión dada.
showExpA :: ExpA -> String
showExpA = foldExpA c s p
  where
    c = show
    s r1 r2 = r1 ++ "+" ++ r2
    p r1 r2 = "(" ++ r1 ++ "*" ++ r2 ++ ")"

-- evalExpA :: ExpA -> Int
-- evalExpA (Cte n) = n
-- evalExpA (Suma e1 e2) = evalExpA e1 + evalExpA e2
-- evalExpA (Prod e1 e2) = evalExpA e1 * evalExpA e2

esUno :: ExpA -> Bool
esUno exp = evalExpA exp == 1

esCero :: ExpA -> Bool
esCero (Cte n) = n == 0
esCero (Suma e1 e2) = evalExpA e1 == 0 && evalExpA e2 == 0
esCero (Prod e1 e2) = evalExpA e1 == 0 || evalExpA e2 == 0

-- recExpA :: (b -> [b] -> b -> b) -> (b -> [b] -> b -> b) -> (Int -> b) -> ExpA -> b

-- foldExpA :: (Int -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExpA -> b
-- foldExpA f g h (Cte x) = f x
-- foldExpA f g h (Suma t1 t2) = g (foldExpA f g h t1) (foldExpA f g h t2)
-- foldExpA f g h (Prod t1 t2) = h (foldExpA f g h t1) (foldExpA f g h t2)

-- recExpA c s p (Cte x) = c x
-- recExpA c s p (Suma t1 t2) = s t1 t2 (recExpA c s p t1) (recExpA c s p t2)
-- recExpA c s p (Prod t1 t2) = p t1 t2 (recExpA c s p t1) (recExpA c s p t2)

recExpA :: (Int -> b) -> (ExpA -> ExpA -> b -> b -> b) -> (ExpA -> ExpA -> b -> b -> b) -> ExpA -> b
recExpA fcte fsuma fprod (Cte x) = fcte x
recExpA fcte fsuma fprod (Suma t1 t2) = fsuma t1 t2 (recExpA fcte fsuma fprod t1) (recExpA fcte fsuma fprod t2)
recExpA fcte fsuma fprod (Prod t1 t2) = fprod t1 t2 (recExpA fcte fsuma fprod t1) (recExpA fcte fsuma fprod t2)

-- que describe la cantidad de constructores de suma con al menos uno de sus hijos constante cero
cantDeSumaCeros :: ExpA -> Int
cantDeSumaCeros = recExpA c s p
  where
    c n = 0
    s nr1 nr2 r1 r2 = unoSiEsCero nr1 + unoSiEsCero nr2 + r1 + r2
    p nr1 nr2 r1 r2 = 0

-- que describe la cantidad de constructores de producto con al menos uno de sus hijos constante uno.
cantDeProdUnos :: ExpA -> Int
cantDeProdUnos = recExpA c s p
  where
    c n = 0
    s nr1 nr2 r1 r2 = r1 + r2
    p nr1 nr2 r1 r2 = unoSiEsUno nr1 + unoSiEsUno nr2 + r1 + r2

unoSiEsUno (Cte 1) = 1
unoSiEsUno _ = 0

unoSiEsCero (Cte 0) = 1
unoSiEsCero _ = 0

-- 2

data EA = Const Int | BOp BinOp EA EA deriving (Show)

data BinOp = Sum | Mul deriving (Show)

foldEA :: (Int -> t) -> (BinOp -> t -> t -> t) -> EA -> t
foldEA f g (Const x) = f x
foldEA f g (BOp x t1 t2) = g x (foldEA f g t1) (foldEA f g t2)

-- que describe si la expresión dada no tiene números negativos de manera explícita.
noTieneNegativosExplicitosEA :: EA -> Bool
noTieneNegativosExplicitosEA = foldEA s c
  where
    s n = n >= 0
    c op r1 r2 = r1 && r2

-- que describe una expresión con el mismo significado que la dada,
-- pero que no tiene sumas del número 0 ni multiplicaciones por 1 o por 0.
-- La resolución debe ser exclusivamente simbólica.
simplificarEA' :: EA -> EA
simplificarEA' = foldEA Const simplificarBinOp
  where
    simplificarBinOp Sum (Const 0) expr = expr
    simplificarBinOp Sum expr (Const 0) = expr
    simplificarBinOp Mul (Const 0) _ = Const 0
    simplificarBinOp Mul _ (Const 0) = Const 0
    simplificarBinOp Mul (Const 1) expr = expr
    simplificarBinOp Mul expr (Const 1) = expr
    simplificarBinOp op expr1 expr2 = BOp op expr1 expr2

-- que describe el número que resulta de evaluar la cuenta representada por la expresión aritmética dada.
evalEA' :: EA -> Int
evalEA' = foldEA s c
  where
    s n = n
    c Sum r1 r2 = r1 + r2
    c Mul r1 r2 = r1 * r2

-- que describe el string sin espacios y con paréntesis correspondiente a la expresión dada
showEA :: EA -> String
showEA = foldEA show c
  where
    c Sum r1 r2 = r1 ++ "+" ++ r2
    c Mul r1 r2 = "(" ++ r1 ++ "*" ++ r2 ++ ")"

-- que describe una expresión aritmética representada con el tipo ExpA,
-- cuyo significado es el mismo que la dada.
ea2ExpA' :: EA -> ExpA
ea2ExpA' = foldEA Cte c
  where
    c Sum r1 r2 = Suma r1 r2
    c Mul r1 r2 = Prod r1 r2

-- 3

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

foldT :: b -> (a -> b -> b -> b) -> Tree a -> b
foldT s c EmptyT = s
foldT s c (NodeT x e1 e2) = c x (foldT s c e1) (foldT s c e2)


recT :: (a -> Tree a -> Tree a -> b -> b -> b) -> b -> Tree a -> b
recT c s EmptyT = s
recT c s (NodeT x e1 e2) = c x e1 e2 (recT c s e1) (recT c s e2)


mapT :: (a -> b) -> Tree a -> Tree b
mapT f = foldT EmptyT c
  where
    c x = NodeT (f x)

sumT :: Tree Int -> Int
sumT = foldT 0 c
  where
    c x r1 r2 = x + r1 + r2

sizeT :: Tree a -> Int
sizeT = foldT 0 c
  where
    c x r1 r2 = 1 + r1 + r2

heightT :: Tree a -> Int
heightT = foldT 0 c
  where
    c x r1 r2 = 1 + max r1 r2

preOrder :: Tree a -> [a]
preOrder = foldT [] c
  where
    c x r1 r2 = x : r1 ++ r2

inOrder :: Tree a -> [a]
inOrder = foldT [] c
  where
    c x r1 r2 = r1 ++ [x] ++ r2

postOrder :: Tree a -> [a]
postOrder = foldT [] c
  where
    c x r1 r2 = r1 ++ r2 ++ [x]

mirrorT :: Tree a -> Tree a
mirrorT = foldT EmptyT c
  where
    c x r1 r2 = NodeT x r2 r1

countByT :: (a -> Bool) -> Tree a -> Int
countByT p = foldT 0 c
  where
    c x r1 r2 = if p x then 1 + r1 + r2 else r1 + r2

partitionT :: (a -> Bool) -> Tree a -> ([a], [a])
partitionT p = foldT ([], []) c
  where
    c x r1 r2 =
      let p1 = fst r1 ++ fst r2
          p2 = snd r1 ++ snd r2
       in if p x
            then (x : p1, p2)
            else (p1, x : p2)

zipWithT :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
zipWithT f = foldT s c
  where
    s _ = EmptyT
    c x r1 r2 (NodeT y yr1 yr2) = NodeT (f x y) (r1 yr1) (r2 yr2)

caminoMasLargo :: Tree a -> [a]
caminoMasLargo = foldT s c
  where
    s = []
    c x r1 r2 = x : masLarga r1 r2

masLarga :: [a] -> [a] -> [a]
masLarga l1 l2 = if length l1 >= length l2 then l1 else l2

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos = foldT s c
  where
    s = []
    c x [] [] = [[x]]
    c x r1 r2 = map (x:) (r1 ++ r2)

todosLosNiveles :: Tree a -> [[a]]
todosLosNiveles = foldT s c
  where
    s = []
    c x [] [] = [[x]]
    c x r1 r2 = [x] : concatPorNivel r1 r2

concatPorNivel [] ys = ys
concatPorNivel xs [] = xs
concatPorNivel (x:xs) (y:ys) = (x++y) : concatPorNivel xs ys

nivelN :: Tree a -> Int -> [a]
nivelN t n = foldT s c t n
  where
      s n = []
      c x r1 r2 0 = [x]
      c x r1 r2 n =  r1 (n-1) ++ r2 (n-1)

tree1 = NodeT 1 (NodeT 2 EmptyT EmptyT) (NodeT 3 EmptyT EmptyT)
tree2 = NodeT 4 (NodeT 5 EmptyT EmptyT) (NodeT 6 EmptyT EmptyT)


data Color = AMARILLO | VERDE
data Premio = 
  B        -- El premio es el valor de la ficha 
  | Izq     -- El premio es el valor de la ficha por el puntaje del hijo izquierdo
  | Der     -- El premio es el valor de la ficha por el puntaje del hijo derecho
  | IzqDer  -- El premio es el valor de la ficha por la suma del puntaje de ambos hijos
data Jugador = Verde | Amarillo
data Ficha = F Jugador Color Int
data Treeble = N Color Premio (Maybe Ficha) Treeble Treeble | H (Maybe Ficha)
data Dir = IZQ | DER
data Jugada = J [Dir] Ficha
-- Se modifica el nodo actual cambiando su color y premio por los dados y se continúa
-- desde el hijo dado por la dirección con el resto de las modificaciones dadas
data ModRama =  NoOP | PonerY (Color, Premio) (Dir, ModRama)

hojaT :: Treeble
hojaT = H Nothing

-- El puntaje de un nodo es el valor de la ficha puesta mas el puntaje de sus dos hijos.
-- Solo se calcula el puntaje de un nodo si es el jugador el que jugo, sino el puntaje es cero
-- Los nodos hoja (los ultimos) siempre tienen el premio OB es decir su valor es solo el de la ficha
-- Para que una jugada sea valida tiene que pasar lo siguiente:
-- 1. El nodo tiene que tener el mismo color que la ficha jugada
-- 2. El nodo en donde se pone la ficha de la jugada puede o no tener otra ficha.
-- En caso de tener ficha la ficha a poner tiene que tener un numero mayor al de la ficha puesta.

jugadaValida :: Jugada -> Treeble -> Bool
jugadaValida (J ds f) = jugadaValidaAux ds f

-- Función auxiliar para navegar en el árbol y verificar la validez de la jugada
jugadaValidaAux :: [Dir] -> Ficha -> Treeble -> Bool
jugadaValidaAux [] ficha (H mficha) = validaFichaH mficha ficha
jugadaValidaAux _ _ (H mf) = False
jugadaValidaAux [] ficha (N col _ mficha _ _) = validarFichaN ficha col mficha
jugadaValidaAux (d:ds) ficha (N col _ mf t1 t2) = case d of
                                                  IZQ -> jugadaValidaAux ds ficha t1
                                                  DER -> jugadaValidaAux ds ficha t2

-- Función auxiliar para verificar si la ficha a colocar es válida en el nodo actual
validaFichaH :: Maybe Ficha -> Ficha -> Bool
validaFichaH Nothing _ = True
validaFichaH (Just fichaExistente) fichaNueva = valorFicha fichaNueva > valorFicha fichaExistente

validarFichaN :: Ficha -> Color -> Maybe Ficha -> Bool
validarFichaN (F j c n) c' Nothing = mismoColor c c'
validarFichaN (F j c n) c' (Just (F _ _ m)) = mismoColor c c' && n > m

mismoColor :: Color -> Color -> Bool
mismoColor AMARILLO AMARILLO = True
mismoColor VERDE VERDE = True
mismoColor _ _ = False

colorFicha :: Ficha -> Color
colorFicha (F _ c _) = c

valorFicha :: Ficha -> Int
valorFicha (F _ _ n) = n

puntaje :: Jugador -> Treeble -> Int
puntaje j (H mf) = puntajeH j mf
puntaje j (N c p mf t1 t2) = let pt1 = puntaje j t1
                                 pt2 = puntaje j t2 in
                              puntajeN j p mf pt1 pt2 + pt1 + pt2

puntajeN :: Jugador -> Premio -> Maybe Ficha -> Int -> Int -> Int
puntajeN j p Nothing pt1 pt2 = 0
puntajeN j p (Just (F j' c n)) pt1 pt2 = if mismoJugador j j'
                                          then puntajeNP p n pt1 pt2
                                          else 0

puntajeNP :: Premio -> Int -> Int -> Int -> Int
puntajeNP B n pj1 pj2 = n 
puntajeNP Izq n pj1 pj2 = n * pj1
puntajeNP Der n pj1 pj2 = n * pj2
puntajeNP IzqDer n pj1 pj2 = n * (pj1 + pj2)

puntajeH :: Jugador -> Maybe Ficha -> Int
puntajeH j Nothing = 0
puntajeH j (Just (F j' _ n)) = if mismoJugador j j' then n else 0

mismoJugador :: Jugador -> Jugador -> Bool
mismoJugador Verde Verde = True
mismoJugador Amarillo Amarillo = True
mismoJugador _ _ = False

modifT :: ModRama -> Treeble -> Treeble
modifT NoOP t = t
modifT (PonerY (c,p) (IZQ,is)) (H mf) = N c p mf (modifT is hojaT) hojaT
modifT (PonerY (c,p) (IZQ,is)) (N _ _ mf t1 t2) = N c p mf (modifT is t1) t2
modifT (PonerY (c,p) (DER,is)) (H mf) = N c p mf hojaT (modifT is hojaT)
modifT (PonerY (c,p) (DER,is)) (N _ _ mf t1 t2) = N c p mf t1 (modifT is t2)

modifTF

armarT :: [ModRama] -> Treeble
armarT [] = hojaT
armarT (mr:mrs) = modifT mr (armarT mrs)

extenderT :: Treeble -> [Dir] -> [ModRama] -> Treeble
externerT (H Nothing) [] = armarT
extenderT (N c p mf t1 t2) (d:ds) mrs = case d of
                                          IZQ -> N c p mf (extenderT t1 ds mrs) t2
                                          DER -> N c p mf t1 (extenderT t2 ds mrs)

foldTR :: (Maybe Ficha -> b) -> (Color -> Premio -> Maybe Ficha -> b -> b -> b) -> Treeble -> b
foldTR fh fn (H mf) = fh mf
foldTR fh fn (N c p mf t1 t2) = fn c p mf (foldTR fh fn t1) (foldTR fh fn t2)

recTR :: (Maybe Ficha -> b) -> (Treeble -> Treeble -> Color -> Premio -> Maybe Ficha -> b -> b -> b) -> Treeble -> b
recTR fh fn (H mf) = fh mf
recTR fh fn (N c p mf t1 t2) = fn t1 t2 c p mf (recTR fh fn t1) (recTR fh fn t2)

-- data ModRama =  NoOP | PonerY (Color, Premio) (Dir, ModRama)
foldMR :: b -> (Color -> Premio -> Dir -> b -> b) -> ModRama -> b
foldMR fno fpy NoOP = fno
foldMR fno fpy (PonerY (c,p) (d,mr)) = fpy c p d (foldMR fno fpy mr)

recMR :: b -> (Color -> Premio -> Dir -> ModRama -> b -> b) -> ModRama -> b
recMR fno fpy NoOP = fno
recMR fno fpy (PonerY (c,p) (d,mr)) = fpy c p d mr (recMR fno fpy mr)