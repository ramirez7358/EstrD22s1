module Set (Set, emptyS, addS, belongs
               , removeS, set2list) where

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

data Set a  = S (Tree a)
  {- INV.REP.: en (S t), t cumple ser un BST -}

emptyS   :: Set a  -- O(1)
emptyS = S EmptyT

addS     :: Ord a => a -> Set a -> Set a  -- O(n) (o O(log n) en promedio)
addS x (S t) = S (insertarBST x t)

belongs  :: Ord a => a -> Set a -> Bool   -- O(n) (o O(log n) en promedio)
belongs x (S t) = buscarBST x t

removeS  :: Ord a => a -> Set a -> Set a
removeS x (S t) = S (borrarBST x t)

set2list :: Ord a => Set a -> [a]  -- O(n^2) o O(n) según corresponda...
set2list (S t) = inorder t  -- Consecuencia del Inv.Rep.: ¡la lista está ordenada!

-- ==========================
-- Auxiliares
-- ==========================
buscarBST :: Ord a => a -> Tree a -> Bool  -- O(n), en peor caso
                                           -- pero es O(log n) en promedio
  -- PRECOND: el árbol es BST
buscarBST x EmptyT          = False
buscarBST x (NodeT y ti td) =
    if (x == y)      then True
     else if (x < y) then buscarBST x ti
                     else buscarBST x td

insertarBST :: Ord a => a -> Tree a -> Tree a  -- O(n), en peor caso
                                               -- pero es O(log n) en promedio
  -- PRECOND: el árbol es BST
insertarBST x EmptyT          = NodeT x EmptyT EmptyT
insertarBST x (NodeT y ti td) =
    if (x==y)        then NodeT x ti td
     else if (x < y) then NodeT y (insertarBST x ti) td
                     else NodeT y ti (insertarBST x td)

borrarBST :: Ord a => a -> Tree a -> Tree a
  -- PRECOND: el árbol es BST
borrarBST _ EmptyT          = EmptyT
borrarBST x (NodeT y ti td) =
    if (x==y)        then rearmarBST ti td
     else if (x < y) then NodeT y (borrarBST x ti) td
                     else NodeT y ti (borrarBST x td)

rearmarBST :: Ord a => Tree a -> Tree a -> Tree a
  -- PRECOND: los dos árboles son BSTs
rearmarBST EmptyT td = td
rearmarBST ti     td = let (m, ti') = splitMaxBST ti
                        in NodeT m ti' td

splitMaxBST :: Ord a => Tree a -> (a, Tree a)
  -- PRECOND: el árbol es BST, y NO está vacío
splitMaxBST (NodeT x ti EmptyT) = (x, ti)  
splitMaxBST (NodeT x ti td)     = let (m, td') = splitMaxBST td
                                   in (m, NodeT x ti td')

inorder :: Tree a -> [a]    -- O(n^2), si (++) es O(n)
                            -- pero O(n), si (++) es O(1)
inorder EmptyT          = []
inorder (NodeT x ti td) = inorder ti ++ [x] ++ inorder td
