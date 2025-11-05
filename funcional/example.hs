import System.IO (hFlush, stdout)

-- Funciones puras
suma :: Int -> Int -> Int
suma x y = x + y

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

esPar :: Int -> Bool
esPar n = n `mod` 2 == 0

-- Programa principal con menú
main :: IO ()
main = do
    putStrLn "=== Menú de utilidades ==="
    putStrLn "1. Sumar dos números"
    putStrLn "2. Calcular factorial"
    putStrLn "3. Verificar si un número es par"
    putStrLn "4. Salir"
    putStr "Elige una opción: "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> do
            putStr "Primer número: "
            hFlush stdout
            a <- readLn
            putStr "Segundo número: "
            hFlush stdout
            b <- readLn
            putStrLn ("Resultado: " ++ show (suma a b))
            main
        "2" -> do
            putStr "Número: "
            hFlush stdout
            n <- readLn
            putStrLn ("Resultado: " ++ show (factorial n))
            main
        "3" -> do
            putStr "Número: "
            hFlush stdout
            n <- readLn
            putStrLn (if esPar n then "Es par" else "No es par")
            main
        "4" -> putStrLn "Adiós!"
        _   -> do
            putStrLn "Opción inválida."
            main
