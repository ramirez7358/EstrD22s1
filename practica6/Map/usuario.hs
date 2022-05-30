import Map

fromJust :: Maybe a -> a
  -- PRECOND: no puede ser Nothing
fromJust (Just x) = x 

map0 = assocM 6 110 $
       assocM 5 101 $
       assocM 4 100 $
       assocM 3 11  $
       assocM 2 10  $
       assocM 1 1   $
       assocM 0 0 emptyM

-- Propósito: obtiene los valores asociados a cada clave del map.
-- O(n^2) siendo n la longitud de la lista de claves adentro del map
valuesM :: Eq k => Map k v -> [Maybe v]
valuesM m = valuesL (keys m) m

-- Costo: O(n^2) siendo n la longitud de la lista de claves
-- Se hace una operacion lineal (lookupM) por cada elemento
valuesL :: Eq k => [k] -> Map k v -> [Maybe v]
valuesL [] m = []
valuesL (k:ks) m = lookupM k m : valuesL ks m

-- Propósito: indica si en el map se encuentran todas las claves dadas.
-- Costo: O(n^2)
todasAsociadas :: Eq k => [k] -> Map k v -> Bool
todasAsociadas ks m = todasAsociadaL ks (keys m)

-- Costo: O(n^2) siendo n la longitud de la primera lista.
-- Se hace una operacion lineal (elem) por cada elemento.
todasAsociadaL :: Eq k => [k] -> [k] -> Bool
todasAsociadaL [] _ = True
todasAsociadaL (k:ks) ks2 = elem k ks2 && todasAsociadaL ks ks2

-- Propósito: convierte una lista de pares clave valor en un map.
-- Costo: O(n^2) siendo n la longitud de la lista de tuplas.
-- Y se hace assocM O(n) por cada uno de los elementos
listToMap :: Eq k => [(k, v)] -> Map k v
listToMap [] = emptyM
listToMap ((f,s):tpls) = assocM f s (listToMap tpls)

-- Propósito: convierte un map en una lista de pares clave valor
-- Costo: O(n^2)
-- Se hace una operacion linea y una cuadratica, por lo tanto el costo es cuadratico.
mapToList :: Eq k => Map k v -> [(k, v)]
mapToList m = mapToListOfTuple (keys m) m

-- Costo: O(n^2) siendo n la longitud de la lista de claves
-- Se hace una operacion lineal (lookupM) por cada elemento.
mapToListOfTuple :: Eq k => [k] -> Map k v -> [(k,v)]
mapToListOfTuple [] _ = []
mapToListOfTuple (k:ks) m = (k, fromJust(lookupM k m)) : mapToListOfTuple ks m

-- Propósito: dada una lista de pares clave valor, 
-- agrupa los valores de los pares que compartan la misma clave.
-- Costo: O()
-- Agregar si no esta?
agruparEq :: Eq k => [(k,v)] -> Map k [v]
agruparEq [] = emptyM
agruparEq ((f,s):tpls) =  let map   = agruparEq tpls
                              value = lookupM f map
                          in
                            case value of
                              Nothing   -> assocM f ([s]) map
                              (Just xs) -> assocM f (s:xs) map


-- Propósito: dada una lista de claves de tipo k y un map que 
-- va de k a Int, le suma uno a
-- cada número asociado con dichas claves.
-- Costo: O(n^2) siendo n la longitud de la lista de claves a incrementar
-- Porque: Por cada clave a incrementar se hace una operacion lineal (lookupM)
incrementar :: Eq k => [k] -> Map k Integer -> Map k Integer
incrementar [] m = m
incrementar (x:xs) m = let map = incrementar xs m in
                        assocM x (fromJust(lookupM x map) + 1) map

-- Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo.
-- Si una clave del primero existe en el segundo, es reemplazada por la del primero.
mergeMaps :: Eq k => Map k v -> Map k v -> Map k v
mergeMaps m1 m2 =  mergeMapsL (keys m1) m1 m2

mergeMapsL :: Eq k => [k] -> Map k v -> Map k v -> Map k v
mergeMapsL [] _ m2 = m2
mergeMapsL (x:xs) m1 m2 = let map = mergeMapsL xs m1 m2
                              val = lookupM x m1
                              in
                                assocM x (fromJust val) map