module TermometroV2
  (Termometro, nuevoT, ingresarT
  , sinTempsT, ultimaT, quitarUltimaT, maxT)
 where

data TalVezTemp = NoHayTemp | Temp Int    
-- data Maybe a = Nothing   | Just a
-- El conjunto (Maybe Int) = { Nothing, Just 0, Just 1, Just 2, ... }

data Termometro = TT [Int] (Maybe Int)
  {- INV.REP.: en TT ts m
      * si ts es vacía, m es Nothing
      * si ts NO es vacía, m es (Just t) con t el máximo elemento de ts
      * ¿¿¿el primer elemento de la lista es el último ingresado???      NO ES INVARIANTE
      * ¿¿¿la lista no puede tener una cantidad negativa de elementos??? NO ES INVARIANTE
    OBSERVACIÓN:
      en esta implementación, el primer elemento de la lista va a guardar el último ingresado
  -}

{-
-- VÁLIDOS:
TT [1,2,3] (Just 3)
TT []      Nothing
-- INVÁLIDOS: no respetan el invariante    
TT [1,2,3] Nothing
TT [1,2,3] (Just 10)
TT [1,2,3] (Just 2)
TT []      (Just 0)
-- La tercera condición NO es invariante
TT [1,2,3] (Just 3)  -- ¿Cómo saber si 1 es o no el último ingresado?
-}

nuevoT        :: Termometro
ingresarT     :: Int -> Termometro -> Termometro
sinTempsT     :: Termometro -> Bool
ultimaT       :: Termometro -> Int        -- PARCIAL
quitarUltimaT :: Termometro -> Termometro -- PARCIAL
maxT          :: Termometro -> Int        -- PARCIAL

nuevoT                  = TT []        Nothing
ingresarT t   (TT ts m) = TT (t:ts)    (nuevoMaximoAlIngresar t m)
sinTempsT     (TT ts m) = null ts      -- isNothing m
ultimaT       (TT ts _) = head ts
quitarUltimaT (TT ts m) = TT (tail ts) (nuevoMaximoAlQuitar ts m)
maxT          (TT ts m) = fromJust m   -- el Inv.Rep. GARANTIZA que esto está bien

-- fromJust (Just x) = x

nuevoMaximoAlIngresar :: Int -> Maybe Int -> Maybe Int
nuevoMaximoAlIngresar t Nothing   = Just t
nuevoMaximoAlIngresar t (Just t') = Just (max t t')

nuevoMaximoAlQuitar :: [Int] -> Maybe Int -> Maybe Int
-- PRECOND: la lista no puede ser vacía, y m no puede ser Nothing
nuevoMaximoAlQuitar (t:ts) (Just t') =
    if (null ts) 
     then Nothing
     else if t==t' 
           then Just (maximum ts)
           else Just t' 
