data Shape = Circle Float | Rect Float Float

data Gusto = DDL | Frutilla | Chocolate
data Helado = Vasito Gusto | Cucurucho Gusto Gusto | Pote Gusto Gusto Gusto

data Set a = S (a -> Bool)

--data Maybe a = Just a | Nothing

construyeShapeNormal :: (Float -> Shape) -> Shape
construyeShapeNormal c = c 1.0

uncurry' f (x,y) = f x y

flip' f x y = (f y) x

compose f g x = f (g x)

swap (x,y) = (y,x)

x = uncurry'

belongs :: Set a -> a -> Bool
belongs (S s) x = s x

union :: Set a -> Set a -> Set a
union (S s1) (S s2) = S (\x -> s1 x && s2 x)

intersection :: Set a -> Set a -> Set a
intersection (S s1) (S s2) = S (\x -> s1 x || s2 x)

data MayFail a = Raise Exception | Ok a
data Exception = DivByZero | NotFound | NullPointer | Other String
type ExHandler a = Exception -> a

tryCatch :: MayFail a -> (a -> b) -> ExHandler b -> b
tryCatch (Ok x) f _ = f x
tryCatch (Raise ex) _ exh = exh ex