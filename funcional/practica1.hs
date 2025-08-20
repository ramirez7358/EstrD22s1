-- Ejercicio 1
-- 1 + 1 + 1 + 1
-- cuadruple 1
-- doble (4 - 2)
-- cuadruple (2 - 1)

doble x = x + x

cuadruple x = 4 * x

twice f = g
    where g x = f (f x)

-- Ejercicio 2
-- doble (doble 2)
-- doble 2 + doble 2
-- 2 + 2 + 2 + 2
-- 8

-- Ejercicio 3

-- cuadruple 2
-- 4 * 2   --> def de cuadruple, con x <- 2
-- 8.      --> arit.

-- cuadruple (cuadruple 2)
-- 4 * (cuadruple 2)        --> def. de cuadruple, con x <- cuadruple 2
-- 4 * (4 * 2)              --> def. de cuadruple, con x <- 2
-- 4 * 8                    --> aritm.
-- 32                       --> aritm.

-- Ejercicio 4
triple x = x + x + x

succ x = x + 1

sumarDos x = x + 2

-- Ejercicio 5
-- twice succ = sumarDos

-- twice succ 2 = sumarDos 2
-- succ (succ 2) = 2 + 2
-- succ 3 = 4
-- 4 = 4


-- Ejercicio 6

-- cuadruple = twice doble
-- doble = twice succ
-- doble = (\x -> 2 * x)
-- (\x -> doble x) = twice succ

-- Ejercicio 7
-- twice f = g
--    where g x = f (f x)

-- ((twice twice) doble) 3
-- (g doble) 3                     --> def. de twice, con f = twice
-- (twice (twice doble)) 3         --> def. de g, con x = doble
-- g 3                             --> def. de twice, con f = (twice doble)
-- twice doble (twice doble 3)     --> def. de g, con x = 3
-- g (twice doble 3)               --> def. de twice, con f = doble
-- doble doble (twice doble 3)     --> def. de g, con x = twice doble 3
-- doble doble (g 3)               --> def. de twice, con f = doble
-- doble doble (doble (doble 3))   --> def. de g, con x = 3
-- doble doble (doble (3 + 3))     --> def. de doble, con x = 3
-- doble doble ((3 + 3) + (3 + 3)) --> def. de doble, con x = 3 + 3
-- doble doble 12                  --> aritm.
-- doble (12 + 12)                 --> def. de doble, con x = 12
-- (12 + 12) + (12 + 12)           --> def. de doble, con x = 12 + 12
-- 48                              --> aritm.

-- Ejercicio 8
tripleL = \x -> x + x + x
succL = \x -> x + 1
sumarDosL = \x -> x + 2
twiceL = \f x -> f(f x)
twiceTwiceL = (\f -> (\x -> (f(f(f(f x))))))

-- Ejercicio 9
-- f x = let (y,z) = (x,x) in y --> \x -> x
-- f (x,y) = let z = x + y in g (z,y) where g (a,b) = a - b --> \x, y -> x
-- f p = case p of (x,y) -> x --> \(x, y) -> x
-- f = \p -> let (x,y) = p in y --> f = \(x, y) -> y