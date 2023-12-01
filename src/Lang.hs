module Lang where

data Movimiento = Movimiento Pieza Casilla Casilla Captura Jaque

type PiezaJugador = (Pieza, Jugador)

data Pieza =
    A -- Alfil
  | C -- Caballo
  | D -- Dama
  | P -- Peon
  | R -- Rey
  | T -- Torre
  deriving Read

data Jugador = B | N -- Blancas | Negras
data Jaque = J | M | Nada
type Captura = Bool
type Columna = Char
type Fila = Int
type Casilla = (Columna, Fila)
type Tablero = [[Maybe PiezaJugador]]
