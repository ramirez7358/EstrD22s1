multiplicaYSumaCurry :: Int -> (Int -> Int)
multiplicaYSumaCurry x = \y -> (x * 2) + (y * 2)

bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0  

calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2  

-- twice f = g
--    where g x = f (f x)

twice f x = f (f x)

first (x,y) = x

--apply f = g
--    where g x = f x

apply' f x = f x

compose f g x = f (g x)

swap (x,y) = (y,x)

-- uflip f = g
--    where g p = f (swap p)

uflip f p = f (swap p)

doble x = x * 2

suma x = \y -> x + y

test :: Char -> Int
test 'a' = 1
test _ = 0


-- 
--const x = g
--    where g y = x

const' x y = x

appDup f =  g
    where g x = f (x,x)

appFork (f, g) = h
    where h x = (f x, g x)

appPar (f, g) = h
    where h (x, y) = (f x, g y)

appDist f = g
    where g (x, y) = (f x, f y)

-- flipv2 f = h
--    where h x = k
--            where k y = (f y) x

flipv2 f x y = f y x

subst f = h
    where h g = k
              where k x = (f x) (g x)

id' x = x

curry' f x y = f (x,y)
uncurry' f (x,y) = f x y

many :: Int -> (a -> a) -> a -> a
many 0 f x = x
many n f x = f (many (n-1) f x)

many' :: Int -> (a -> a) -> a -> a
many' 0 f x = compose f id x
many' n f x = compose (many' (n - 1) f) f x

-- Practica 5

data DigBin = O | I
data DigDec = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9

data Gusto = Chocolate | Vainilla | Sambayon | DulceDeLeche | Frutilla deriving Show
data Helado = Vasito Gusto | Cucurucho Gusto Gusto | Pote Gusto Gusto Gusto deriving Show

dbAsInt :: DigBin -> Int
dbAsInt O = 0
dbAsInt I = 1

dbOfBool :: Bool -> DigBin
dbOfBool True = I
dbOfBool False = O

ddAsInt :: DigDec -> Int
ddAsInt D0 = 0
ddAsInt D1 = 1
ddAsInt D2 = 2
ddAsInt D3 = 3
ddAsInt D4 = 4
ddAsInt D5 = 5
ddAsInt D6 = 6
ddAsInt D7 = 7
ddAsInt D8 = 8
ddAsInt D9 = 9

ddOfInt :: Int -> DigDec
ddOfInt 0 = D0
ddOfInt 1 = D1
ddOfInt 2 = D2
ddOfInt 3 = D3
ddOfInt 4 = D4
ddOfInt 5 = D5
ddOfInt 6 = D6
ddOfInt 7 = D7
ddOfInt 8 = D8
ddOfInt 9 = D9

nextDD :: DigDec -> DigDec
nextDD D9 = D0
nextDD d = ddOfInt (succ (ddAsInt d))

prevDD :: DigDec -> DigDec
prevDD D0 = D9
prevDD d = ddOfInt (ddAsInt d - 1)


data Medida = Mm Float | Cm Float | Inch Float | Foot Float deriving (Show)

asMm :: Medida -> Medida
asMm (Cm x) = Mm (x * 10)
asMm (Inch x) = Mm (x * 25.4)
asMm (Foot x) = Mm (x * 304.8)
asMm m = m

asCm :: Medida -> Medida
asCm (Mm m) = Cm (m / 10)
asCm (Inch m) = Cm(m * 2.54)
asCm (Foot m) = Cm(m * 30.48)
asCm m = m

asInch :: Medida -> Medida
asInch (Mm m) = Inch (m / 25.4)
asInch (Cm m) = Inch (m / 2.54)
asInch(Foot m) = Inch (m * 12)
asInch m = m

asFoot :: Medida -> Medida
asFoot (Mm m) = Foot (m * 304.8)
asFoot (Cm m) = Foot (m * 30.48)
asFoot (Inch m) = Foot (m * 12)
asFoot m = m


data Shape = Circle Float | Rect Float Float deriving Show

construyeShapeNormal :: (Float -> Shape) -> Shape
construyeShapeNormal c = c 1.0

data Set a = S (a -> Bool)

pares :: Set Int
pares = S even

enterosPositivos :: Set Int
enterosPositivos = S (> 0)

belongs :: Set a -> a -> Bool
belongs (S f) = f

empty :: Set a
empty = S (const False)

singleton :: Eq a => a -> Set a
singleton x = S(==x)

union :: Set a -> Set a -> Set a
union (S f) (S g) = S(\x -> f x || g x)

intersection :: Set a -> Set a -> Set a
intersection (S f) (S g) = S(\x -> f x && g x)

data MayFail a = Raise Exception | Ok a
data Exception = DivByZero | NotFound | NullPointer | Other String
type ExHandler a = Exception -> a

tryCatch :: MayFail a -> (a -> b) -> ExHandler b -> b
tryCatch (Ok x) f _ = f x
tryCatch (Raise e) _ eh = eh e

chocoHelate c = c Chocolate