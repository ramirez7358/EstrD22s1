module Queue
    (Queue, emptyQ, isEmptyQ, enqueueQ, firstQ, dequeueQ) 
  where

data Queue a = Q [a] Int
  {- INV.REP.: en (Q xs n), n es la longitud de xs -}

emptyQ   :: Queue a
isEmptyQ :: Queue a -> Bool
enqueue  :: a -> Queue a -> Queue a
firstQ   :: Queue a -> a       -- PARCIAL: la cola no es vacía
dequeue  :: Queue a -> Queue a -- PARCIAL: la cola no es vacía
lenQ     :: Queue a -> Int

emptyQ             = Q [] 0               -- O(1)
isEmptyQ (Q _ n)   = n==0                 -- O(1)
enqueue x (Q xs n) = Q (xs++[x]) (n+1)    -- O(n)
                              -- DEBO repestar el invariante
firstQ (Q xs _)    = head xs              -- O(1)
dequeue (Q xs n)   = Q (tail xs) (n-1)    -- O(1)
                              -- DEBO repestar el invariante

lenQ (Q _ n) = n                          -- O(1)
   -- Válido gracias al invariante de representación
