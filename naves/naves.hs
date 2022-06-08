data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]

data Barril = Comida | Oxigeno | Torpedo | Combustible

data Nave = N (Map SectorId Sector) (Map Nombre Tripulante) (MaxHeap Tripulante)

construir :: [SectorId] -> Nave
construir ss = N (construirS ss) emptyM emptyH


-- O(K log K)
construirS :: [SectorId] -> Map SectorId Sector
constuirS [] = emptyM
constuirS (s:ss) = assocM s (crearS s) (construirS ss)

-- Propósito: Incorpora un tripulante a la nave, sin asignarle un sector.
-- Eficiencia: O(log T)
ingresarT :: Nombre -> Rango -> Nave -> Nave
ingresarT n r (N s nt ts) = let tripulante = crearT n r in
                                N s (assocM n tripulante) (insertH tripulante ts)


-- Propósito: Devuelve los sectores asignados a un tripulante.
-- Precondición: Existe un tripulante con dicho nombre.
-- Eficiencia: O(log M)
sectoresAsignados :: Nombre -> Nave -> Set SectorId
sectoresAsignados n (N _ nt _) = case (lookupM n nt) of
                                    Nothing -> error "No hay tripulante"
                                    Just t -> sectoresT t



-- Propósito: Asigna una lista de componentes a un sector de la nave.
-- Eficiencia: O(C + log S), siendo C la cantidad de componentes dados.
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector cs sid (N ss nt ht) = case (lookupM sid ss) of
                                    Nothing     -> (N ss nt ht)
                                    Just sector -> (N (assocM sid (componentesS sector ++ cs) ss) nt ht)


-- Propósito: Devuelve todos los sectores no vacíos (con tripulantes asignados)
-- El costo de domM es absorbido por el O(K log K) de sectoresNoVacios
sectores :: Nave -> Set SectorId
sectores (N ss _ _) = sectoresNoVacios (domM ss) ss -- O(log K)

-- O(K log k + N log N)
-- Siendo K la longitud de la lista de sectores id
-- Siendo N la cantidad de claves distintas en el map
sectoresNoVacios :: [SectorId] -> Map SectorId Sector -> Set SectorId
sectoresNoVacios [] _ = emptyS
sectoresNoVacios (s:ss) m = let sector = lookupM s m                    -- O(log K)
                                tripulantes = tripulantesS sector       -- O(1)
                            in
                                if emptyS tripulantes                   -- O(1)
                                    then sectoresNoVacios ss m
                                    else addS s (sectoresNoVacios ss m) -- O(log N)




-- Propósito: Devuelve los tripulantes que no poseen sectores asignados.
sinSectoresAsignados :: Nave ->[Tripulante]
sinSectoresAsignados (N _ mt _) = sinSectoresAsignadosL (domM mt in) mt  -- O(K)

-- O(K log K) siendo K la cantidad de nombres distintos en el map
sinSectoresAsignadosL :: [Nombre] -> Map Nombre Tripulante -> [Tripulante]
sinSectoresAsignadosL [] _ = []
sinSectoresAsignadosL (n:ns) m = case (lookupM n) of                                -- O(log K)
                                    Nothing -> sinSectoresAsignadosL ns m
                                    Just t  -> if emptyS (sectoresT t)              -- O(1)
                                                then t : sinSectoresAsignadosL ns m
                                                else sinSectoresAsignadosL ns m

-- Propósito: Devuelve todos los barriles de los sectores asignados de la nave
barriles :: Nave -> [Barril]
barriles (N ss _ _) = barrilesM (donM ss) ss

barrilesM :: [SectorId] -> Map SectorId Sector -> [Barril]
barrilesM [] _ = []
barrilesM (s:ss) m = case (lookupM s m) of                                      -- O(log K)
                        Nothing -> barrilesM ss m
                        Just s  -> barrilesC (componentesS s) ++ barrilesM ss m -- O(n)


-- O(n)
barrilesC :: [Componentes] -> [Barril]
barrilesC (c:cs) = barrilSiHay c ++ barrilesC cs  

-- O(1)
barrilSiHay :: Componente -> [Barril]
barrilSiHay (Almacen barriles) = barriles
barrilSiHay _ = []



-- Propósito: Asigna un sector a un tripulante.
-- Nota: No importa si el tripulante ya tiene asignado dicho sector.
-- Precondición: El tripulante y el sector existen.
-- Eficiencia: O(log S + log T + T log T)
asignarASector :: Nombre -> SectorId -> Nave -> Nave
asignarASector n sid (N _ mnt _) =  let tripulante = fromJust (lookupM sid mnt)
                                        tripulanteConSector = asignarS sid tripulante
                                    in (N _ (assocM n tripulanteConSector mnt) _)


-- Propósito: Devuelve la lista de tripulantes ordenada por rango, de mayor a menor.
-- Eficiencia: O(log T)
tripulantesN :: Nave -> [Tripulante]

