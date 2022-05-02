module QueueV2
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
queue e (Queue xs) = Queue (e:xs)

-- O(n)
firstQ :: Queue a -> a
firstQ (Queue xs) = last xs

-- O(n)
dequeue :: Queue a -> Queue a
dequeue (Queue xs) = Queue (init xs)