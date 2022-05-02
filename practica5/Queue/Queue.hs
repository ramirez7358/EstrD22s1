module Queue
    (Queue, emptyQ, isEmptyQ, queue, firstQ, dequeue)
where

data Queue a = Queue [a] deriving Show

-- O(1)
emptyQ :: Queue a
emptyQ = Queue []

-- O(1)
isEmptyQ :: Queue a -> Bool
isEmptyQ (Queue xs) = null xs

-- O(1)
queue :: a -> Queue a -> Queue a
queue e (Queue xs) = Queue (xs ++ [e])

-- O(1)
firstQ :: Queue a -> a
firstQ (Queue xs) = head xs

-- O(1)
dequeue :: Queue a -> Queue a
dequeue (Queue (x:xs)) = Queue xs