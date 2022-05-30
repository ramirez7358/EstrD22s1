import PriorityQueue
import Map
--import Multiset
import SetV2
import Persona

{-
-- n es 3 (porque no importan repetidos en un conjunto)
--  removeS es lineal, ¿pero respecto de QUÉ n?
-- En la implementación CON repetidos, ¿qué costo tiene removeS?
--  [17,17,17,17,...,17,17,17,17,17,17,17,17,99,42]
--              1M de 17s
--  PERO N es 1M
ej = addS 17
   $ addS 17
   ...       -- 1M de 17s
   $ addS 17
   $ addS 17
   $ addS 17
   $ addS 17
   $ addS 17
   $ addS 17
   $ addS 17
   $ addS 17
   $ addS 17
   $ addS 17
   $ addS 99 
   $ addS 42 
   $ emptyS
-}

instance Eq Persona where
  p1 == p2 = edad p1 == edad p2

instance Ord Persona where
  p1 <= p2 = edad p1 <= edad p2

instance Show Persona where
  show p = "Persona { nombre <- "   ++ show (nombre p)
                ++ ", apellido <- " ++ show (apellido p)
                ++ ", edad <- "     ++ show (edad p)
                ++ " }"

ff       = crearPersona "Fidel"     "ML"      53
ale      = crearPersona "Alejandro" "Castro"  36
cristian = crearPersona "Cristian"  "Sottile" 29
juan     = crearPersona "Juan"      "Plotuki" 29

crearPersona :: String -> String -> Int -> Persona
crearPersona n a e = crecerVeces e (nacer n a)

crecerVeces :: Int -> Persona -> Persona
crecerVeces 0 p = p
crecerVeces n p = crecer (crecerVeces (n-1) p)

ejPQ = insertPQ ff
     $ insertPQ ale
     $ insertPQ cristian
     $ emptyPQ

instance (Show a, Ord a) => Show (PriorityQueue a) where
  -- show emptyPQ = "{{}}"  -- NOOOOOOO. NO SE PUEDE HACER PM SOBRE LA INTERFAZ DE UN TAD. Sos usuario...
  show pq = if isEmptyPQ pq 
             then " ||"
             else " << " ++ show (findMinPQ pq) ++ show (deleteMinPQ pq)
   -- Si deleteMinPQ o findMinPQ es O(n), entonces show es O(n^2)             
   -- Si deleteMinPQ y findMinPQ es O(1), entonces show es O(n)


instance (Show k, Show v, Ord k) => Show (Map k v) where
  -- show (Map k v) = ...   -- WAT??  MEZCLAMOS TIPO con NOMBRES de variables... :(
  show m = "{ " ++ showAssocs (domM m) m ++ " }"
    where showAssocs []     _ = "<emptyM>"
          showAssocs [k]    m = showAssoc k m
          showAssocs (k:ks) m = showAssoc k m ++ ", " ++ showAssocs ks m
          showAssoc k m = show k ++ " -> " ++ show (fromJust (lookupM k m))

fromJust :: Maybe a -> a
  -- PRECOND: no puede ser Nothing
fromJust (Just x) = x          


ejM = assocM "FF"       53
    $ assocM "Ale"      36
    $ assocM "Cristian" 29
    $ emptyM 