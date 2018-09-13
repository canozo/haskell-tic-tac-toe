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
ganador :: Char -> [Char] -> Bool
ganador jugador lista =
  rev_horizontal 0 0 0 jugador lista ||
  rev_vertical 0 0 0 jugador lista ||
  rev_diagonal 0 0 0 jugador lista

-- revisar si hay ganador de manera horizontal
-- variables: pos, num de caracteres repetidos, num iteracion, char a buscar, lista, resultado
rev_horizontal :: Int -> Int -> Int -> Char -> [Char] -> Bool
rev_horizontal pos cuenta iteracion jugador lista =
  if pos == 16 then
    -- se sale del valor maximo de la lista
    False
  else if lista !! pos == jugador then
    -- si encuentra una casilla con el mismo valor X/0 del jugador
    if cuenta == 2 then
      -- contando la casilla actual, se encontraron 3 seguidos
      True
    else if iteracion < 3 then
      -- todavia no llega a los tres seguidos
      rev_horizontal (pos + 1) (cuenta + 1) (iteracion + 1) jugador lista
    else
      -- cambia de fila, empieza denuevo desde 0
      rev_horizontal (pos + 1) 0 0 jugador lista
  else if iteracion == 3 then
    -- cambia de fila, empieza denuevo desde 0
    rev_horizontal (pos + 1) 0 0 jugador lista
  else
    -- no encontro una casilla con el mismo valor X/0 del jugador
    rev_horizontal (pos + 1) 0 (iteracion + 1) jugador lista

-- revisar si hay ganador de manera vertical
-- variables: pos, num de caracteres repetidos, num iteracion, char a buscar, lista, resultado
rev_vertical :: Int -> Int -> Int -> Char -> [Char] -> Bool
rev_vertical pos cuenta iteracion jugador lista =
  if lista !! pos == jugador then
    -- si encuentra una casilla con el mismo valor X/0 del jugador
    if cuenta == 2 then
      -- contando la casilla actual, se encontraron 3 seguidos
      True
    else if iteracion < 3 then
      -- todavia no llega a los tres seguidos
      rev_vertical (pos + 4) (cuenta + 1) (iteracion + 1) jugador lista
    else
      -- en este caso es posible de que haya un desfase no deseado, lo verificamos:
      if (pos + 1 - 4 * 3) /= 4 then
        -- cambia de columna, empieza denuevo desde 0
        rev_vertical (pos + 1 - 4 * 3) 0 0 jugador lista
      else
        -- estaria buscando la columna 5 (no existe)
        False
  else if iteracion == 3 then
    -- en este caso es posible de que haya un desfase no deseado, lo verificamos:
    if (pos + 1 - 4 * 3) /= 4 then
      -- cambia de columna, empieza denuevo desde 0
      rev_vertical (pos + 1 - 4 * 3) 0 0 jugador lista
    else
      -- estaria buscando la columna 5 (no existe)
      False
  else
    -- no encontro una casilla con el mismo valor X/0 del jugador
    rev_vertical (pos + 4) 0 (iteracion + 1) jugador lista

-- revisar si hay ganador de manera diagonal, de izquierda a derecha
-- variables: pos, num de caracteres repetidos, num iteracion, char a buscar, lista, resultado
rev_diagonal_id :: Int -> Int -> Int -> Char -> [Char] -> Bool
rev_diagonal_id pos cuenta iteracion jugador lista =
  False

-- revisar si hay ganador de manera diagonal, de derecha a izquierda
-- variables: pos, num de caracteres repetidos, num iteracion, char a buscar, lista, resultado
rev_diagonal_di :: Int -> Int -> Int -> Char -> [Char] -> Bool
rev_diagonal_di pos cuenta iteracion jugador lista =
  False

-- modo yisus: revisar todos los casos posibles
rev_diagonal :: Char -> [Char] -> Bool
rev_diagonal jugador lista =
  (lista !! 1  == jugador   &&
   lista !! 6  == jugador   &&
   lista !! 11 == jugador)  ||

  (lista !! 4  == jugador   &&
   lista !! 9  == jugador   &&
   lista !! 14 == jugador)  ||

  (lista !! 5  == jugador   &&
   lista !! 10 == jugador   &&
  (lista !! 0  == jugador   ||
   lista !! 15 == jugador)) ||

  (lista !! 2  == jugador   &&
   lista !! 5  == jugador   &&
   lista !! 8  == jugador)  ||

  (lista !! 7  == jugador   &&
   lista !! 10 == jugador   &&
   lista !! 13 == jugador)  ||

  (lista !! 6  == jugador   &&
   lista !! 9  == jugador   &&
  (lista !! 3  == jugador   ||
   lista !! 12 == jugador))

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

