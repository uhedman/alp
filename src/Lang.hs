module Lang where

type Pos = (Int, Int)
type Table = [(Pos, State)]

type State = Char
type Neighbour = (Orientation, State)
type Game = ([State], State -> [Neighbour] -> State, String)
data Orientation = N | E | S | W | NE | SE | SW | NW deriving Eq
data Boundary = Open | Closed | Periodic | Reflective deriving (Eq, Show)