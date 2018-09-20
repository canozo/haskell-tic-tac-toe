import Graphics.UI.Gtk

-- Retornar una lista con un elemento reemplazado
-- ejemplos:
-- cambiar_elemento 2 'X' ['X', '0', '0']
-- devuelve ['X', '0', 'X']
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
  rev_diagonal jugador lista


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


-- revisar todos los casos posibles
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


mover :: (ButtonClass o) => o -> Int -> o -> [o] -> [Char] -> IO ()
mover button num titulo botones tablero = do
  label_anterior <- buttonGetLabel button
  if label_anterior == " " then
    -- es un movimiento valido
    set button [buttonLabel := "X"]
    -- ver si gano
    -- hacer movimiento de computadora
    -- ver si la compu gano
  else
    -- la casilla ya esta ocupada
    set button [buttonLabel := label_anterior]


main :: IO ()
main = do
  initGUI

  -- ventana
  window  <- windowNew

  -- vbox principal
  vbox    <- vBoxNew True 0

  -- todos los hbox
  hbox0    <- hBoxNew True 0
  hbox1    <- hBoxNew True 0
  hbox2    <- hBoxNew True 0
  hbox3    <- hBoxNew True 0
  hbox4    <- hBoxNew True 0

  -- etiqueta de ganador
  button0  <- buttonNewWithLabel "X/0"

  -- todos los botones
  button1  <- buttonNewWithLabel " "
  button2  <- buttonNewWithLabel " "
  button3  <- buttonNewWithLabel " "
  button4  <- buttonNewWithLabel " "
  button5  <- buttonNewWithLabel " "
  button6  <- buttonNewWithLabel " "
  button7  <- buttonNewWithLabel " "
  button8  <- buttonNewWithLabel " "
  button9  <- buttonNewWithLabel " "
  button10 <- buttonNewWithLabel " "
  button11 <- buttonNewWithLabel " "
  button12 <- buttonNewWithLabel " "
  button13 <- buttonNewWithLabel " "
  button14 <- buttonNewWithLabel " "
  button15 <- buttonNewWithLabel " "
  button16 <- buttonNewWithLabel " "

  -- hacer un arreglo de caracteres que
  let tablero = "                "

  -- hacer un arreglo con todos los botones
  let botones = [button1,
                 button2 ,
                 button3 ,
                 button4 ,
                 button5 ,
                 button6 ,
                 button7 ,
                 button8 ,
                 button9 ,
                 button10,
                 button11,
                 button12,
                 button13,
                 button14,
                 button15,
                 button16]

  -- ventana con vbox como el child principal
  set window [windowDefaultWidth   := 200 ,
              windowDefaultHeight  := 200 ,
              containerBorderWidth := 10  ,
              containerChild       := vbox]

  -- poner todos los hbox dentro del vbox
  boxPackStart vbox hbox0 PackGrow 0
  boxPackStart vbox hbox1 PackGrow 0
  boxPackStart vbox hbox2 PackGrow 0
  boxPackStart vbox hbox3 PackGrow 0
  boxPackStart vbox hbox4 PackGrow 0

  -- poner todos los botones dentro de cada hbox
  boxPackStart hbox0 button0  PackGrow 0
  boxPackStart hbox1 button1  PackGrow 0
  boxPackStart hbox1 button2  PackGrow 0
  boxPackStart hbox1 button3  PackGrow 0
  boxPackStart hbox1 button4  PackGrow 0
  boxPackStart hbox2 button5  PackGrow 0
  boxPackStart hbox2 button6  PackGrow 0
  boxPackStart hbox2 button7  PackGrow 0
  boxPackStart hbox2 button8  PackGrow 0
  boxPackStart hbox3 button9  PackGrow 0
  boxPackStart hbox3 button10 PackGrow 0
  boxPackStart hbox3 button11 PackGrow 0
  boxPackStart hbox3 button12 PackGrow 0
  boxPackStart hbox4 button13 PackGrow 0
  boxPackStart hbox4 button14 PackGrow 0
  boxPackStart hbox4 button15 PackGrow 0
  boxPackStart hbox4 button16 PackGrow 0

  -- acciones de los botones
  onClicked button1  (mover button1  0  button0 botones tablero)
  onClicked button2  (mover button2  1  button0 botones tablero)
  onClicked button3  (mover button3  2  button0 botones tablero)
  onClicked button4  (mover button4  3  button0 botones tablero)
  onClicked button5  (mover button5  4  button0 botones tablero)
  onClicked button6  (mover button6  5  button0 botones tablero)
  onClicked button7  (mover button7  6  button0 botones tablero)
  onClicked button8  (mover button8  7  button0 botones tablero)
  onClicked button9  (mover button9  8  button0 botones tablero)
  onClicked button10 (mover button10 9  button0 botones tablero)
  onClicked button11 (mover button11 10 button0 botones tablero)
  onClicked button12 (mover button12 11 button0 botones tablero)
  onClicked button13 (mover button13 12 button0 botones tablero)
  onClicked button14 (mover button14 13 button0 botones tablero)
  onClicked button15 (mover button15 14 button0 botones tablero)
  onClicked button16 (mover button16 15 button0 botones tablero)

  onDestroy window mainQuit
  widgetShowAll window
  mainGUI

