module Stack
    (Stack, emptyS, isEmptyS, push, top, pop, lenS)
where

data Stack a = Stack [a] deriving Show


-- O(1)
emptyS :: Stack a
emptyS = Stack []

-- O(1)
isEmptyS :: Stack a -> Bool
isEmptyS (Stack xs) = null xs

-- O(1)
push :: a -> Stack a -> Stack a
push e (Stack xs) = Stack (e:xs)

-- O(1)
top :: Stack a -> a
top (Stack xs) = head xs

-- O(1)
pop :: Stack a -> Stack a
pop (Stack xs) = Stack (tail xs)

-- O(1)
lenS :: Stack a -> Int
lenS (Stack xs) = length xs