module SetV1(Set, emptyS, addS, belongs, unionS, set2list) where

data Set a = Set [a] Int
  {- INV.REP.: en (Set xs n)
       * xs no tiene repetidos 
       * n es la longitud de la lista
  -}

emptyS               = Set [] 0                    -- O(1)
addS x (Set xs n)    = Set (agregar x xs)  
                           (verSiAumenta n x xs)   -- O(n)
belongs x (Set xs _) = x `elem` xs                 -- O(n)
unionS (Set xs _) 
       (Set ys _)    = let zs = juntar xs ys
                        in Set zs (length zs)      -- O(n^2)
lenS (Set _ n)       = n                           -- O(1)
set2List (Set xs _)  = xs                          -- O(1)

-- O(n)
verSiAumenta :: Eq a => Int -> a -> [a] -> Int
  -- PROP.: si x está, entonces el mismo, sino, uno más
verSiAumenta n x xs = if x `elem` xs then n else n+1

-- O(n)
agregar x []     = [x]
agregar x (y:ys) = if (x==y) then y:ys else y:agregar x ys

-- O(n^2)
juntar []     ys = ys
juntar (x:xs) ys = agregar x (juntar xs ys)