cuantasVecesMultiplicarPor2Para :: Int -> Int   
cuantasVecesMultiplicarPor2Para 1 = 0   -- PRECOND: n>0
cuantasVecesMultiplicarPor2Para n = 
       1 + cuantasVecesMultiplicarPor2Para (div n 2)

logBase2 :: Int -> Int  -- PRECOND: n>0
logBase2 1 = 0
logBase2 n = 1 + logBase2 (div n 2)
