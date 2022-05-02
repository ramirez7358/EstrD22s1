module QueueV3
    (Queue, emptyQ, isEmptyQ, queue, firstQ, dequeue)
where

data Queue a = Queue [a] [a] deriving Show

-- Invariante de representacion
-- Si fs se encuentra vacía, entonces la cola se encuentra vacía.

-- O(1)
emptyQ :: Queue a
emptyQ = Queue [] []

-- O(1)
isEmptyQ :: Queue a -> Bool
isEmptyQ (Queue fs bs) = null fs

-- O(1)
queue :: a -> Queue a -> Queue a
queue e (Queue fs bs) = if null fs
                            then Queue [x] bs
                            else Queue fs (x:bs)

-- O(n)
firstQ :: Queue a -> a
firstQ (Queue fs bs) = head fs

-- O(n)
--dequeue :: Queue a -> Queue a
--dequeue (Queue xs) = Queue (init xs)