module Lang where

data Movimiento = 
    Normal Pieza (Maybe Casilla) Casilla Captura Jaque
  | EnroqueCorto Jaque
  | EnroqueLargo Jaque
  deriving Show

type PiezaJugador = (Pieza, Jugador)

data Pieza =
    A -- Alfil
  | C -- Caballo
  | D -- Dama
  | P -- Peon
  | R -- Rey
  | T -- Torre
  deriving (Read, Show)

data Jugador = 
  B | N -- Blancas | Negras
  deriving Show
data Jaque = 
  J | JM | SJ -- Jaque | Jaque Mate | Sin Jaque
  deriving Show
type Captura = Bool
type Columna = Char
type Fila = Int
type Casilla = (Columna, Fila)
type Tablero = [[Maybe PiezaJugador]]
