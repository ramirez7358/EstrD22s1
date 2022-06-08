module Map2
    (Map, emptyM, assocM, lookupM, deleteM, keys)
where

-- Como una lista de pares-clave valor con claves repetidas
data Map k v = Map [(k,v)] deriving Show

-- Propósito: devuelve un map vacío
-- Costo: O(1)
emptyM :: Map k v
emptyM = Map []

-- Propósito: agrega una asociación clave-valor al map.
-- Costo: O(1)
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM k v (Map tpls) = Map ((k,v) : tpls)

-- Propósito: encuentra un valor dado una clave.
-- Costo: O(n) con n la cantidad de elementos en la lista de tuplas
lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM k (Map tpls) = lookupL k tpls

lookupL :: Eq k => k -> [(k,v)] -> Maybe v
lookupL _ [] = Nothing
lookupL k ((k2,v2):tpls) = if k == k2
                            then Just v2
                            else lookupL k tpls

-- Propósito: borra una asociación dada una clave.
-- Costo: O(n) siendo n la longitud de tpls
deleteM :: Eq k => k -> Map k v -> Map k v
deleteM k (Map tpls) = Map (deleteL k tpls)

deleteL :: Eq k => k -> [(k,v)] -> [(k,v)]
deleteL _ [] = []
deleteL k ((k2,v2):tpls) = if k == k2
                            then deleteL k tpls
                            else (k2,v2) : (deleteL k tpls)

-- Propósito: devuelve las claves del map
-- Costo: O(n^2) siendo n la longitud de la lista
keys :: Eq k => Map k v -> [k]
keys (Map tpls) = keysL tpls

-- Costo: O(n^2) siendo n la longitud de la lista
keysL :: Eq k => [(k,v)] -> [k]
keysL [] = []
keysL ((k,v):tpls) = agregarSiNoEsta k (keysL tpls)

-- Costo: O(n) siendo en la longitud de la lista de 
-- claves en la que hay que buscar
agregarSiNoEsta :: Eq k => k -> [k] -> [k]
agregarSiNoEsta k [] = k : []
agregarSiNoEsta k ks = if elem k ks
                        then ks
                        else k:ks