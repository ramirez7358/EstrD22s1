module MultiSet
    (MultiSet)
where

import Map

data MultiSet k = MultiSet (Map k Int)

-- Propósito: denota un multiconjunto vacío.
emptyMS :: MultiSet a
emptyMS = MultiSet emptyM

-- Propósito: dados un elemento y un multiconjunto, agrega una
-- ocurrencia de ese elemento al
--addMS :: Ord a => a -> MultiSet a -> MultiSet a
--addMS