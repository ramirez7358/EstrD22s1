module Map
    (Map, emptyM, assocM, lookupM, deleteM, keys)
where

data Map k v = Map [(k,v)] deriving Show

-- Propósito: devuelve un map vacío
-- Costo: O(1)
emptyM = Map []
emptyM :: Map k v

-- Propósito: agrega una asociación clave-valor al map.
-- Costo: O(n)
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM k v (Map tpls) = Map (assocL k v tpls)

-- Propósito: agrega una asociación clave-valor a la lista de tuplas.
-- Costo: O(n) siendo n la longitud de la lista de tuplas.
assocL :: Eq k => k -> v -> [(k,v)] -> [(k,v)]
assocL k v [] = [(k,v)]
assocL k v ((k2, v2): tpls) = if k == k2
                                then (k,v) : tpls
                                else (k2,v2) : assocL k v tpls

-- Propósito: encuentra un valor dado una clave.
-- Costo: O(n) siendo n la longitud de la lista dentro del Map.
lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM k (Map tpls) = lookupL k tpls

-- Costo: O(n) siendo n la longitud de la lista de tuplas.
lookupL :: Eq k => k -> [(k,v)] -> Maybe v
lookupL _ [] = Nothing
lookupL k ((k2,v2):tpls) = if k == k2
                            then Just v2
                            else lookupL k tpls

-- Propósito: borra una asociación dada una clave.
-- Costo: O(n) siendo n la longitud de la lista en el map.
deleteM :: Eq k => k -> Map k v -> Map k v
deleteM k (Map tpls) = Map (deleteL k tpls)

-- O(n) siendo n la longitud de la lista de tuplas
deleteL :: Eq k => k -> [(k,v)] -> [(k,v)]
deleteL _ [] = []
deleteL k ((k2,v2):tpls) = if k == k2
                            then tpls
                            else (k2,v2) : deleteL k tpls

-- Propósito: devuelve las claves del map.
-- Costo: O(n) siendo n la longitud de la lista dentro del map.
keys :: Map k v -> [k]
keys (Map tpls) = keysL tpls

-- Costo: O(n) siendo n la longitud de la lista de tuplas.
keysL :: [(k,v)] -> [k]
keysL [] = []
keysL ((k,_):tpls) = k : keysL tpls