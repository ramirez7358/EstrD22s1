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


recExpA c s p (Cte x) = c x
recExpA c s p (Suma t1 t2) = s t1 t2 (recExpA c s p t1) (recExpA c s p t2)
recExpA c s p (Prod t1 t2) = p t1 t2 (recExpA c s p t1) (recExpA c s p t2)

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

data EA = Const Int | BOp BinOp EA EA deriving Show
data BinOp = Sum | Mul deriving Show


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

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)