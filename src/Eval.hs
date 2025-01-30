module Eval ( execSet, execStep ) where
import MonadAC
    ( getCols, getEpoch, getRows, getTable, getGame, modify, MonadAC, printAC, getBoundary )
import Lang ( Pos, Table, State, Neighbour, Orientation (..), Boundary (..) )
import Global ( GlEnv(table, isHalted, epoch) )
import Data.Maybe (fromMaybe)
import Data.List (nub)

execSet :: MonadAC m => (Pos, State) -> m ()
execSet (pos, state) = do
  r <- getRows
  c <- getCols
  if isInBound r c pos 
  then do tab <- getTable
          let newEntry = (pos, state)
              updatedTable = newEntry : filter (\(p, _) -> p /= pos) tab
          modify (\s -> s {table = updatedTable, isHalted = False})
  else printAC "Put fuera del tablero."

isInBound :: Int -> Int -> Pos -> Bool
isInBound rows cols (a, b) = a >= 1 && b >= 1 && a <= rows && b <= cols

evalStep :: (State -> [Neighbour] -> State) -> Boundary -> Table -> Int -> Int -> Table
evalStep f bound tab rows cols = filter (\(_, s) -> s /= ' ') (map processCell cells)
  where
    -- Celdas que hay que procesar
    cells :: [Pos]
    cells = nub (concatMap (neighbs . fst) tab)

    -- Esta función procesará cada celda de la tabla.
    processCell :: Pos -> (Pos, State)
    processCell pos = (pos, f (lookupState pos) (neighbours pos))

    neighbs :: Pos -> [Pos]
    neighbs (r, c) = case bound of
          Closed -> filter inbounds positions
          Open -> positions
          Periodic -> map per positions
          Reflective -> filter inbounds positions
      where
      positions = [(r-1, c-1), (r-1, c), (r-1, c+1),
                  (r, c-1),   (r, c),   (r, c+1),
                  (r+1, c-1), (r+1, c), (r+1, c+1)]
      
      inbounds = isInBound rows cols

    neighbours :: Pos -> [Neighbour]
    neighbours (r, c) = 
      case bound of
        Closed -> [(NW, lookupState (r-1, c-1)), (N, lookupState (r-1, c)), (NE, lookupState (r-1, c+1)),
                   (W, lookupState (r, c-1)),                               (E, lookupState (r, c+1)),
                   (SW, lookupState (r+1, c-1)), (S, lookupState (r+1, c)), (SE, lookupState (r+1, c+1))]
        Open -> [(NW, lookupState (r-1, c-1)), (N, lookupState (r-1, c)), (NE, lookupState (r-1, c+1)),
                 (W, lookupState (r, c-1)),                               (E, lookupState (r, c+1)),
                 (SW, lookupState (r+1, c-1)), (S, lookupState (r+1, c)), (SE, lookupState (r+1, c+1))]
        Periodic -> [(NW, lookupState (per (r-1, c-1))), (N, lookupState (per (r-1, c))), (NE, lookupState (per (r-1, c+1))),
                     (W, lookupState (per (r, c-1))),                                     (E, lookupState (per (r, c+1))),
                     (SW, lookupState (per (r+1, c-1))), (S, lookupState (per (r+1, c))), (SE, lookupState (per (r+1, c+1)))]
        Reflective -> [(NW, lookupState (max 1 (r-1), max 1 (c-1))),    (N, lookupState (max 1 (r-1), c)),    (NE, lookupState (max 1 (r-1), min cols (c+1))),
                       (W, lookupState (r, max 1 (c-1))),                                                     (E, lookupState (r, min cols (c+1))),
                       (SW, lookupState (min rows (r+1), max 1 (c-1))), (S, lookupState (min rows (r+1), c)), (SE, lookupState (min rows (r+1), min cols (c+1)))]

    -- Función para obtener el estado de una celda dada (usando la tabla original).
    lookupState :: Pos -> State
    lookupState pos = fromMaybe ' ' (lookup pos tab)
    
    per :: Pos -> Pos
    per (x, y) = let x' = ((x - 1) `mod` rows) + 1
                     y' = ((y - 1) `mod` cols) + 1
                 in (x', y')

execStep :: MonadAC m => m ()
execStep = do tab <- getTable
              e <- getEpoch
              rows <- getRows
              cols <- getCols
              (_, f, _) <- getGame
              bound <- getBoundary
              let newTable = evalStep f bound tab rows cols
              modify (\s -> if newTable == tab
                            then s {isHalted = True}
                            else s {epoch = e + 1, table = newTable})
