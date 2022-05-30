module Set
    (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)
where

data Set a = Set [a] Int deriving Show

--Crea un conjunto vacío.
-- O(1)
emptyS :: Set a
emptyS = Set [] 0

--Dados un elemento y un conjunto, agrega el elemento al conjunto.
-- O(n)
addS :: Eq a => a -> Set a -> Set a
addS e (Set ns l) = if elem e ns
                    then Set ns l
                    else Set (e:ns) (l+1)

--Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
belongs :: Eq a => a -> Set a -> Bool
belongs e (Set xs _) = belongsList e xs

-- O(n) 
belongsList :: Eq a => a -> [a] -> Bool
belongsList _ [] = False
belongsList n (x:xs) = n == x || belongsList n xs

-- Devuelve la cantidad de elementos distintos de un conjunto.
sizeS :: Eq a => Set a -> Int
sizeS (Set _ l) = l

-- Borra un elemento del conjunto.
-- PRECOND: El elemento que se quiere borrar existe en la lista
removeS :: Eq a => a -> Set a -> Set a
removeS n (Set xs l) = Set (removeL n xs) (l-1)

removeL :: Eq a => a -> [a] -> [a]
removeL _ [] = error "El elemento no esta en la lista"
removeL n (x:xs) = if n == x
                    then xs
                    else x : removeL n xs

-- Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos conjuntos
unionS :: Eq a => Set a -> Set a -> Set a
unionS (Set xs _) (Set ys _) = let list = unionL xs ys in Set list (length list)

unionL :: Eq a => [a] -> [a] -> [a]
unionL [] ys = ys
unionL (x:xs) (ys) = if elem x ys
                    then unionL xs ys
                    else x : unionL xs ys

-- Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
setToList :: Eq a => Set a -> [a]
setToList (Set xs _) = xs