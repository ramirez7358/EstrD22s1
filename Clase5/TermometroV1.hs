module TermometroV1
  (Termometro, nuevoT, ingresarT
  , sinTempsT, ultimaT, quitarUltimaT, maxT)
 where

data Termometro = T [Int]

nuevoT        :: Termometro
ingresarT     :: Int -> Termometro -> Termometro
sinTempsT     :: Termometro -> Bool
ultimaT       :: Termometro -> Int        -- PARCIAL
quitarUltimaT :: Termometro -> Termometro -- PARCIAL
maxT          :: Termometro -> Int        -- PARCIAL

nuevoT               = T []
ingresarT   t (T ts) = T (t:ts)
sinTempsT     (T ts) = null ts
ultimaT       (T ts) = head ts
quitarUltimaT (T ts) = T (tail ts)
maxT          (T ts) = maximum ts
