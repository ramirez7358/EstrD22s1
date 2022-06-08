--[
--    [1,2,3,4],
--    [3,4,5,6]
--]

-- Dado una matriz devuelve el grado de la misma --
grado :: [[Int]] -> (Int, Int)
grado rs = (length rs, length (head rs))

-- Dado un grado de matriz devuelve la matriz nula para ese grado
nula :: (Int, Int) -> [[Int]]
nula (r,c) = repetirNVeces r (repetirNVeces c 0)

-- Dado un grado devuelve la matriz identidad para ese grado
identidad :: (Int, Int) -> [[Int]]
identidad (r,c) = repetirNVeces r (repetirNVeces c 0)

repetirNVeces :: Int -> a -> [a]
repetirNVeces 0 e = []
repetirNVeces n e = e : repetirNVeces (n-1) e