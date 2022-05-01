module SetV2 
    (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)
where

data Set a = Set [a] deriving Show

-- O(1)
emptyS :: Set a
emptyS = Set []

-- O(1)
addS :: Eq a => a -> Set a -> Set a
addS x (Set xs) = Set(x:xs)

-- O(n)
belongs :: Eq a => a -> Set a -> Bool
belongs e (Set xs) = belongsList e xs

-- O(n)
belongsList :: Eq a => a -> [a] -> Bool
belongsList _ [] = False
belongsList e (x:xs) = e == x || belongsList e xs

-- O(n)
sizeS :: Eq a => Set a -> Int
sizeS (Set xs) = sizeL xs

-- O(n)
sizeL :: Eq a => [a] -> Int
size [] = 0;
sizeL (x:xs) = 1 + sizeL xs

-- O(n)
removeS :: Eq a => a -> Set a -> Set a
removeS e (Set xs) = Set (removeL e xs)

-- O(n)
removeL :: Eq a => a -> [a] -> [a]
removeL e (x:xs) = if e == x
                    then xs
                    else x : removeL e xs

-- O(1)
unionS :: Eq a => Set a -> Set a -> Set a
unionS (Set xs) (Set ys) = Set (xs ++ ys)

setToList :: Eq a => Set a -> [a]
setToList (Set xs) = eliminarRepetidos xs

eliminarRepetidos :: Eq a => [a] -> [a]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) = agregarSiNoEsta x (eliminarRepetidos xs)

agregarSiNoEsta :: Eq a => a -> [a] -> [a]
agregarSiNoEsta x xs = if elem x xs
                        then xs
                        else x:xs


