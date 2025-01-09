module Lang where

data Action =
    Kill (Int, Int)
  | Revive (Int, Int)

type Pos = (Int, Int)
type Table = [Pos]
