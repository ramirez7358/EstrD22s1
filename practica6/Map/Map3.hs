module Map3
    (Map, emptyM, assocM, lookupM, deleteM, keys)
where

-- Como dos listas, una de claves y otra de valores, donde la clave 
-- ubicada en la posición i está
-- asociada al valor en la misma posición, pero de la otra lista.
data Map k v = Map [k] [v] deriving Show

--INVARIANTE DE REPRESENTACION:
-- *Cada elemento en la lista de k es la key que se corresponde al valor 
--del elemento en la lista de a que se ubica en la misma posición.
-- *No pueden haber dos k repetidos en la lista.

-- Propósito: devuelve un map vacío
-- Costo: O(1)
emptyM :: Map k v
emptyM = Map [] []

-- Propósito: agrega una asociación clave-valor al map.
-- Costo: O(1)
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM k v (Map ks vs) = Map (k:ks) (v:vs)

-- Propósito: encuentra un valor dado una clave.
lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM k (Map ks vs) = lookupL k ks vs

lookupL :: Eq k => k -> [k] -> [v] -> Maybe v
lookupL _ [] _ = Nothing
lookupL k (k2:k2s) (v2:v2s) = if k == k2
                                then Just v2
                                else lookupL k k2s v2s

-- Propósito: borra una asociación dada una clave.
deleteM :: Eq k => k -> Map k v -> Map k v
deleteM x (Map [] _) = emptyM
deleteM x (Map (k:ks) (v:vs)) = if x == k
                                    then deleteM x (Map ks vs)
                                    else assocM k v (deleteM x (Map ks vs))

-- Propósito: devuelve las claves del map.
keys :: Map k v -> [k]
keys (Map ks _) = ks