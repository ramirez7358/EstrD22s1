data NExp = Var Variable
                    | NCte Int
                    | NBOp NBinOp NExp NExp
data NBinOp = Add | Sub | Mul | Div | Mod | Pow
type Variable = String

-- cantExtra :: NExp -> Int
-- cfNE :: NExp -> NExp
-- Prop: ¿cantExtra . cfNE = const 0?
-- cantExtra :: NExp -> Int ==> Devuelve la cantidad extra de sub-NExp que se podrian optimizar. 
-- cfNE :: NExp -> NExp ==> Devuelve una NExp sin expresiones optimizables