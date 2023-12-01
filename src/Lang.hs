module Lang where

data Movimiento =
    Captura Pieza Casilla
  | CapturaConJaque Pieza Casilla
  | Jaque Pieza Casilla
  | JaqueMate Pieza Casilla

data Pieza =
    A Jugador -- Alfil
  | C Jugador -- Caballo
  | D Jugador -- Dama
  | P Jugador -- Peon
  | R Jugador -- Rey
  | T Jugador -- Torre

data Jugador = B | N -- Blancas | Negras
type Columna = Char
type Fila = Int
type Casilla = (Columna, Fila)
type Tablero = [Maybe Pieza]
