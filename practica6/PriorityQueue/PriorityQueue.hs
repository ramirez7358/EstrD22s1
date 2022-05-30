module PriorityQueue
    (PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ)
where

data PriorityQueue a = PriorityQueue [a] deriving Show


-- Propósito: devuelve una priority queue vacía.
-- Coscto: O(1)
emptyPQ :: PriorityQueue a
emptyPQ = PriorityQueue []

-- Propósito: indica si la priority queue está vacía.
-- Costo: O(1)
isEmptyPQ :: PriorityQueue a -> Bool
isEmptyPQ (PriorityQueue xs) = null xs

-- Propósito: inserta un elemento en la priority queue.
-- Costo
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
insertPQ e (PriorityQueue xs) = PriorityQueue (e:xs)

-- Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
-- Precondición: parcial en caso de priority queue vacía.
-- Costo: O(n) siendo n la longitud de la cola
findMinPQ :: Ord a => PriorityQueue a -> a
findMinPQ (PriorityQueue xs) = minimum xs

-- Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo).
-- Precondición: parcial en caso de priority queue vacía.
-- O(n) 
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
deleteMinPQ (PriorityQueue xs) = PriorityQueue (deleteL (minimum xs) xs)

-- Costo: O(n)
deleteL :: Ord a => a -> [a] -> [a]
deleteL _ [] = []
deleteL e (x:xs) = if x == e
                    then xs
                    else x : deleteL e xs