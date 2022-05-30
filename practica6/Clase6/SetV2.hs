module SetV2(Set, emptyS, addS, belongs, unionS, set2list) where

data Set a = Set [a] Int
  {- INV.REP.: en (Set xs n)
       * n es la cantidad de elementos DISTINTOS de xs
  -}

emptyS                   = Set [] 0                    -- O(1)
addS x (Set xs n)        = Set (x : xs)
                               (verSiAumenta n x xs)   -- O(N), N y n NO SON EL MISMO!!!!!!
belongs x (Set xs _)     = x `elem` xs                 -- O(N), N es la cantidad de elementos de xs!!
unionS (Set xs _) 
       (Set ys _)        = Set (xs ++ ys)
                               (length (juntar xs ys)) -- O(N^2)
lenS (Set xs n)          = n                           -- O(1)
set2list (Set xs _)      = sinRepetidos xs             -- O(N^2)

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

sinRepetidos xs = juntar xs []