data Pizza = Prepizza | Capa Ingrediente Pizza
data Ingrediente = Aceitunas Int | Anchoas | Cebolla | Jamón | Queso | Salsa

cantidadDeAceitunas :: Pizza -> Int
cantidadDeAceitunas Prepizza = 0
cantidadDeAceitunas (Capa i p) = aceitunas i + cantidadDeAceitunas p

aceitunas :: Ingrediente -> Int
aceitunas (Aceitunas n) = n
aceitunas _ = 0

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa i p) = Capa (dupAceitunas i) (duplicarAceitunas p)

dupAceitunas :: Ingrediente -> Ingrediente
dupAceitunas (Aceitunas n) = Aceitunas (n * 2)
dupAceitunas i = i

sinLactosa :: Pizza -> Pizza
sinLactosa Prepizza = Prepizza
sinLactosa (Capa i p) = if esQueso i
                            then sinLactosa p
                            else Capa i (sinLactosa p)

esQueso :: Ingrediente -> Bool
esQueso Queso = True
esQueso _ = False

aptaIntolerantesLactosa :: Pizza -> Bool
aptaIntolerantesLactosa Prepizza = True
aptaIntolerantesLactosa (Capa i p) = not (esQueso i) && aptaIntolerantesLactosa p

conDescripcionMejorada :: Pizza -> Pizza
conDescripcionMejorada Prepizza = Prepizza
conDescripcionMejorada (Capa i p) = juntarAceitunas i (conDescripcionMejorada p)

juntarAceitunas :: Ingrediente -> Pizza -> Pizza
juntarAceitunas (Aceitunas n) (Capa (Aceitunas m) p) = Capa (Aceitunas (n + m)) p
juntarAceitunas i p = Capa i p
