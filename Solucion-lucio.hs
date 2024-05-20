import Data.Char
-- No se permite agrear nuevos imports
-- Sólo está permitido usar estas funciones:
-- https://campus.exactas.uba.ar/pluginfile.php/557895/mod_resource/content/1/validas_tp.pdf


-- Completar!
-- Nombre de grupo: {}
-- Integrante1: { DNI1,apellidoYNombre1}
-- Integrante2: { DNI2,apellidoYNombre2}
-- Integrante3: { DNI3,apellidoYNombre3}
-- Integrante4: { DNI4,apellidoYNombre4}
-- Integrantes que abandonaron la materia: {En caso que haya abandonado la materia algún
                        -- integrante, completar con los dni y apellidos, sino dejar vacío}


-- 1

esMinuscula :: Char -> Bool
esMinuscula c | ord c >= 97 && ord c <= 122 = True
              | otherwise = False

-- 2

letraANatural :: Char -> Int
letraANatural c = ord c - ord 'a'

-- 3

desplazar :: Char -> Int -> Char
desplazar c n | not (esMinuscula c) = c
              | otherwise = chr (ord c + calcDesplazamiento c n)

calcDesplazamiento :: Char -> Int -> Int -- calcula el desplazamiento si se pasa del ord a u ord z para armar un bucle
calcDesplazamiento c n 
  | letraANatural c + n > letraANatural 'z' = calcDesplazamiento c (n - 26)
  | letraANatural c + n < letraANatural 'a' = calcDesplazamiento c (n + 26)
  | otherwise = n

-- 4

cifrar :: String -> Int -> String
cifrar [] _ = []
cifrar (c:s) n 
  | not (esMinuscula c) = c:(cifrar s n)
  | otherwise = (desplazar c n):(cifrar s n)

-- 5

descifrar :: String -> Int -> String
descifrar [] _ = []
descifrar (c:s) n
  | not (esMinuscula c) = c:(descifrar s n)
  | otherwise = (desplazar c (-n)):(descifrar s n)

-- 6

cifrarLista :: [String] -> [String]
cifrarLista [] = []
cifrarLista ls = cifrarListaAux ls 0

cifrarListaAux :: [String] -> Int -> [String]
cifrarListaAux [] _ = []
cifrarListaAux (l:ls) n
  | ls == [] = (cifrar l n):ls
  | otherwise = (cifrar l n):(cifrarListaAux ls (n+1))

-- 7

frecuencia :: String -> [Float]
frecuencia s = frecuenciaAux s (length s) 0

frecuenciaAux :: String -> Int -> Int -> [Float]
frecuenciaAux (c:s) longitud i
  | i == 26 = []
  | otherwise = (porcentajeLetra (c:s) longitud i):(frecuenciaAux (c:s) longitud (i+1))

porcentajeLetra :: String -> Int -> Int -> Float
porcentajeLetra [] _ _ = 0
porcentajeLetra (c:s) longitud i
  | letraANatural c == i = division (cuantoAparece c (c:s)) longitud * 100
  | otherwise = porcentajeLetra s longitud i

division :: Int -> Int -> Float
division p q = fromIntegral p / fromIntegral q

cuantoAparece :: Char -> String -> Int -- cuantas veces aparece el caracter en la palabra
cuantoAparece _ [] = 0
cuantoAparece c (s:fs)
  | c == s = 1 + cuantoAparece c fs
  | otherwise = cuantoAparece c fs

-- 8

cifradoMasFrecuente :: String -> Int -> (Char,Float)
cifradoMasFrecuente s n = masFrecAux (frecuencia (cifrar s n)) 0 ('!',(-1))

masFrecAux :: [Float] -> Int -> (Char,Float) -> (Char,Float)
masFrecAux frec i max
  | i == 26 = max
  | iesimoElemento frec i > snd max = masFrecAux frec (i+1) (naturalALetra i, iesimoElemento frec i)
  | otherwise = masFrecAux frec (i+1) max

naturalALetra :: Int -> Char
naturalALetra c = chr(c + ord 'a')

iesimoElemento :: (Eq t) => [t] -> Int -> t
iesimoElemento (x:xs) 0 = x
iesimoElemento (x:xs) n = iesimoElemento xs (n-1)

-- 9

esDescifrado :: String -> String -> Bool
esDescifrado frase cifrada
  | esDesAux frase cifrada 0 = True
  | otherwise = False

esDesAux :: String -> String -> Int -> Bool
esDesAux frase cifrada n
  | n == 26 = False
  | cifrada == cifrar frase n = True
  | otherwise = esDesAux frase cifrada (n+1)

-- 10

todosLosDescifrados :: [String] -> [(String,String)]
todosLosDescifrados frases




