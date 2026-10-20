-- =====================================================================
--  CI-3661 — Tarea 1 (Haskell) — Septiembre–Diciembre 2026
--
--  Nombre: Andres Ramirez
--  Carnet: 21-10520
--
--  Declaración de uso de herramientas de IA (obligatoria, ver enunciado):
--  Gemini modelo 3.8 flash, para entender flags de compilacion 
--
-- =====================================================================
--  No cambies el nombre del módulo ni la lista de exportación, ni las
--  firmas de las funciones. Puedes agregar todas las funciones auxiliares
--  que quieras. El único import permitido es Data.Char.
-- =====================================================================
module Tarea1
  ( esPalindromo
  , mayorYMenor
  , leerHora
  , duracion
  , promedioSobreUmbral
  , movimientosCaballo
  , enDosSaltos
  , miMap
  , miFilter
  , miFoldl
  ) where

import Data.Char (isAlphaNum, isDigit, toLower, digitToInt)

-- =====================================================================
-- Parte A: recursión explícita.
-- Prohibido: map, filter, foldr, foldl, listas por comprensión, reverse,
-- maximum, minimum, all, any y cualquier otra función de orden superior.
-- =====================================================================

-- A1
esPalindromo :: String -> Bool
esPalindromo [] = True
esPalindromo a = string_limpia == reverse_lab string_limpia
  where
    string_limpia = limpiar_string a


-- A2
-- Explica aquí, en dos o tres líneas, por qué tu solución recorre la
-- lista una sola vez:
-- Se divide la lista hasta el caso base [a], realizando O(n) llamadas
-- al regresar las llamadas cada elemento se visita una unica vez actualizando min y max
-- en la tupla en una sola pasada
mayorYMenor :: Ord a => [a] -> Maybe (a, a)
mayorYMenor [] = Nothing
mayorYMenor lista = Just (aux_min_max lista)



-- A3
leerHora :: String -> Either String (Int, Int)
leerHora [] = Left "formato inválido"
leerHora [a1, a2 , ':', b1, b2] 
  | not (isDigit a1) = Left "formato inválido"
  | not (isDigit a2) = Left "formato inválido"
  | not (isDigit b1) = Left "formato inválido"
  | not (isDigit b2) = Left "formato inválido"
  | a_num > 23 = Left "hora fuera de rango"
  | b_num > 59 = Left "minutos fuera de rango"
  | otherwise = Right (a_num, b_num)
  where

    a_num = 10 * (digitToInt a1) + (digitToInt a2)
    b_num = 10 * (digitToInt b1) + (digitToInt b2)
leerHora _ = Left "formato inválido"

duracion :: String -> String -> Either String Int
duracion a b = case (leerHora a,leerHora b) of
  (Right (x0,y0), Right(x1,y1)) ->
    if x1 < x0 then Right ((((24+x1) - x0) * 60 ) + y1 - y0) else Right ((x1-x0) * 60 + y1 - y0)
  (Left err, _) -> Left err
  (_, Left err) -> Left err

-- =====================================================================
-- Parte B: orden superior.
-- Prohibido: recursión explícita (ninguna función de esta parte, ni sus
-- auxiliares, puede llamarse a sí misma).
-- =====================================================================

-- B1
promedioSobreUmbral :: Double -> [Double] -> Maybe Double
promedioSobreUmbral umbral lista =
  let
    resultado_fold = foldr f acc lista
  in 
    case resultado_fold of
      (_, 0.0) ->
        Nothing
      (suma, n) ->
        Just (suma / n)
  where
    acc = (0.0, 0.0)

    f x (suma, n) = 
      if x > umbral then (suma + x, n + 1) else (suma,n)

-- B2
movimientosCaballo :: (Int, Int) -> [(Int, Int)]
movimientosCaballo (x,y) = [ (a, b) | a <- [1..8], b <- [1..8],
  let dx = a - x, let dy = b - y, dx*dx + dy*dy == 5] 

enDosSaltos :: (Int, Int) -> Int
enDosSaltos (x,y) =
  let
    resultado_fold = foldr f acc lista
  in
    snd resultado_fold
  where
    acc = ([] , 0)
    lista = enDosSaltosSucio((x,y))

    f casilla (lista_limpia, suma) = 
      if pertenece casilla lista_limpia then (lista_limpia, suma) else (casilla:lista_limpia , suma + 1)


-- B3
miMap :: (a -> b) -> [a] -> [b]
miMap = undefined

miFilter :: (a -> Bool) -> [a] -> [a]
miFilter = undefined

miFoldl :: (b -> a -> b) -> b -> [a] -> b
miFoldl = undefined


 -- Funciones auxiliares
reverse_lab :: [Char] -> [Char]
reverse_lab [] = []
reverse_lab (x:xs) = reverse_lab xs ++ [x]

limpiar_string :: [Char] -> [Char]
limpiar_string [] = []
limpiar_string (x:xs)
  | isAlphaNum x = toLower x : limpiar_string xs
  | otherwise = limpiar_string xs

aux_min_max :: Ord a => [a] -> (a,a)
aux_min_max []  = error "aux_min_max: lista vacia"
aux_min_max [a] = (a,a)
aux_min_max (x:xs) =
  let
    (max_aux,min_aux) = aux_min_max(xs)

    new_max = if x > max_aux then x else max_aux
    new_min = if x < min_aux then x else min_aux
  in 
    (new_max, new_min)

pertenece :: (Int, Int) -> [(Int, Int)] -> Bool
pertenece a lista = foldr f False lista
  where
    f x acc = (x == a) || acc

enDosSaltosSucio :: (Int, Int) -> [(Int, Int)]
enDosSaltosSucio (x,y) =
  let
    primer_salto = movimientosCaballo (x,y)
    segundo_salto = [(a,b)| (x1,y1) <- primer_salto, (a,b) <- movimientosCaballo(x1,y1)]
  in
    segundo_salto