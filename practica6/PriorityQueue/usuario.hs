import PriorityQueue

--
heapSort :: Ord a => [a] -> [a]
heapSort [] = []
heapSort xs =  pqToList(listToPQ xs)

listToPQ :: Ord a => [a] -> PriorityQueue a
listToPQ [] = emptyPQ
listToPQ (x:xs) = insertPQ x (listToPQ xs)

pqToList :: Ord a => PriorityQueue a -> [a]
pqToList pq = if isEmptyPQ pq
                then []
                else findMinPQ pq : pqToList (deleteMinPQ pq)