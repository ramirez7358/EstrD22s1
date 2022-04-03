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