map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x : xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x : xs) =
  let r = filter' p xs
   in if p x
        then x : r
        else r

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x : xs) = f x (foldr' f z xs)

type Empleado = String

type Empresa = [Empleado]

ausencias :: Empleado -> Int
ausencias e = 1

losSarmiento :: Empresa -> [Empleado]
losSarmiento = filter' ((== 0) . ausencias)

presentismos :: [Empresa] -> [Empresa] -- Empresas pero sin personal sin ausencias
presentismos = filter' (null . losSarmiento)

-- Funciones con foldr

map'' f = foldr' ((:) . f) []

filter'' p = foldr' (\x -> if p x then (x:) else id) []

prod' = foldr' (*) 1