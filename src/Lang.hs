module Lang where
import qualified Data.Map.Strict as Map

type Pos = (Int, Int)
type Table = Map.Map Pos State

type State = Char
type Neighbour = (Orientation, State)
type Game = ([State], State -> [Neighbour] -> State, String)
data Orientation = N | E | S | W | NE | SE | SW | NW deriving Eq
data Boundary = Open | Closed | Periodic | Reflective deriving (Eq, Show)