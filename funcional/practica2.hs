-- recibe dos parametros y devuelve el primero
const x = g
    where g y = x

-- recibe una funcion y un parametro
-- aplica la funcion a una tupla del parametro recibido
appDup f = g
    where g x = f (x, x)

-- recibe un par de funciones y un parametro
-- devuelve un par con los resultados de aplicar las funciones al parametro
appFork (f, g) = h
    where h x = (f x, g x)

-- recibe un par de funciones y un par de parametros
-- devuelve un par con los resultados de aplicar las funciones
-- a los parametros que estan en la misma posicion de la tupla
appPar (f, g) = h
    where h (x, y) = (f x, g y)

-- recibe una funcion y par de valores
-- devuelve un par aplicando la funcion a ambos parametros
appDist f = g
    where g (x, y) = (f x, f y)

-- recibe una funcion y dos parametros
-- devuelve 
flip f = h
    where h x = k
            where k y = (f y) x

-- recibe dos funciones y un parametro
-- aplica las funciones al mismo argumento
-- luego aplica el primer resultado (fx) al segundo resultado (gx)
subst f = h
    where h g = k
            where k x = (f x) (g x)