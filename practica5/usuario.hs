--import Set
import SetV2

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) 
    deriving Show

-- Set --

set0 = addS 15 (addS 10 (addS 1 emptyS))
set1 = addS 80 (addS 15 (addS 2 emptyS))
set2 = addS 45 (addS 76 (addS 3 emptyS))

-- Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen
-- al conjunto.
losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen (x:xs) s = if belongs x s
                                then x : losQuePertenecen xs s
                                else losQuePertenecen xs s

-- Quita todos los elementos repetidos de la
-- lista dada utilizando un conjunto como estructura auxiliar
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos xs = setToList (listToSet xs)

listToSet :: Eq a => [a] -> Set a
listToSet [] = emptyS
listToSet (x:xs) = addS x (listToSet xs) 

-- Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos
-- del arbol.
unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT = emptyS
unirTodos (NodeT s ts1 ts2) = unionS s (unionS (unirTodos ts1) (unirTodos ts2))