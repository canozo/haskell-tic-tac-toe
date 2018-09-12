-- Retornar una lista con un elemento reemplazado
-- ejemplos:
-- cambiar_elemento 2 'X' ['X]
cambiar_elemento :: Int -> Char -> [Char] -> [Char]
cambiar_elemento _ _ [] = []
cambiar_elemento n nuevo_valor (x:xs)
  | n == 0 = nuevo_valor:xs
  | otherwise = x:cambiar_elemento (n - 1) nuevo_valor xs

-- Ver si hay un ganador en una tabla
-- ejemplos:
-- ganador 'X' ['X', ' ', ' ', ' ', 'X', ' ', ' ', ' ', 'X', ' ', ' ', ' ', ' ', ' ', ' ', ' ']
-- devuelve 'X' (porque X gana)
-- ganador 'X' ['0', ' ', ' ', ' ', '0', ' ', ' ', ' ', '0', ' ', ' ', ' ', ' ', ' ', ' ', ' ']
-- devuelve '0' (porque 0 gana)
-- ganador 'X' [' ', ' ', ' ', ' ', '0', ' ', ' ', ' ', '0', ' ', ' ', ' ', ' ', ' ', ' ', ' ']
-- devuelve ' ' (porque nadie gana)
ganador :: Char -> [Char] -> Char
ganador jugador lista =
  if rev_horizontal jugador lista ||
     rev_vertical jugador lista ||
     rev_diagonal jugador lista
  then jugador else ' '

-- revisar si hay ganador de manera horizontal
rev_horizontal :: Char -> [Char] -> Bool
rev_horizontal _ _ = True

-- revisar si hay ganador de manera vertical
rev_vertical :: Char -> [Char] -> Bool
rev_vertical _ _ = True

-- revisar si hay ganador de manera diagonal
rev_diagonal :: Char -> [Char] -> Bool
rev_diagonal _ _ = True

-- Ver si la lista esta llena
-- ejemplos:
-- lleno ['X', '0']
-- devuelve True
-- lleno ['X', ' ']
-- devuelve False
lleno :: [Char] -> Bool
lleno [] = True
lleno (head : tail) = if head == ' ' then False else lleno tail

-- Ver si puede mover y ejecuta el movimiento
-- ejemplos:
-- mover 3 'X' ['X', '0', '0', '0']
-- devuelve ['X', '0', '0', '0']
-- mover 3 'X' ['X', '0', 'X', ' ']
-- devuelve ['X', '0', 'X', 'X']
mover :: Int -> Char -> [Char] -> [Char]
mover pos mov lista = if lista !! pos == ' ' then cambiar_elemento pos mov lista else lista

main = do
  print("Hola")

