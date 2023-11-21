newtype MultiSet a = MS (a -> Int)

-- count :: Eq a => a -> MultiSet a -> Int
-- count x (MS ms) = ms x

add :: (Eq a) => a -> MultiSet a -> MultiSet a
add x (MS ms) = MS (\y -> if y == x then ms y + 1 else ms y)

decrease :: (Eq a) => a -> MultiSet a -> MultiSet a
decrease x (MS ms) = MS (\y -> if y == x then ms y - 1 else ms y)

-- Ejemplo de uso
multiset :: MultiSet Int
multiset = MS (\x -> if x == 3 then 5 else if x == 5 then 2 else 0)

data Letra = A | B

type Palabra = [Letra]

data Trie = N Int Trie Trie | V

tt = N 0 (N 1 (N 3 V V) (N 2 V V)) (N 6 (N 8 V (N 1 V V)) (N 2 V V))

count :: Palabra -> Trie -> Int
count [] _ = 0
count _ V = 0
count (l : ls) (N _ t1 t2) = 1 + count ls (case l of A -> t1; B -> t2)

countPrefix :: Palabra -> Trie -> Int
countPrefix _ V = 0
countPrefix [] (N x t1 t2) = x
countPrefix (l : ls) (N x t1 t2) = countPrefix ls (case l of A -> t1; B -> t2) + x

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x:xs) = f x (foldr' f z xs)
