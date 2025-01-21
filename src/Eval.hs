module Eval ( execSet, execStep ) where
import MonadAC
    ( getCols, getEpoch, getRows, getTable, getGame, modify, MonadAC )
import Lang ( Pos, Table, State, Neighbour, Orientation (..) )
import Global ( GlEnv(table, isHalted, epoch) )
import Data.Maybe (fromMaybe)
import Data.List (nub)

execSet :: MonadAC m => (Pos, State) -> m ()
execSet (pos, state) = do
  tab <- getTable
  let newEntry = (pos, state)
      updatedTable = newEntry : filter (\(p, _) -> p /= pos) tab
  modify (\s -> s {table = updatedTable, isHalted = False})

neighbs :: Int -> Int -> Pos -> [Pos]
neighbs rows cols (x, y) = filter inbounds positions
  where
    positions = [(x-1, y-1), (x, y-1), (x+1, y-1), 
                 (x-1, y),   (x, y),   (x+1, y),
                 (x-1, y+1), (x, y+1), (x+1, y+1)]
    inbounds = precomputedBounds rows cols

precomputedBounds :: Int -> Int -> Pos -> Bool
precomputedBounds rows cols (a, b) = a >= 1 && b >= 1 && a <= rows && b <= cols

evalStep :: (State -> [Neighbour] -> State) -> Table -> Int -> Int -> Table
evalStep f tab rows cols = filter (\(_, s) -> s /= ' ') (map processCell cells)
  where
    -- Celdas que hay que procesar
    cells :: [Pos]
    cells = nub (concatMap (neighbs rows cols . fst) tab)

    -- Esta función procesará cada celda de la tabla.
    processCell :: Pos -> (Pos, State)
    processCell pos = (pos, f (lookupState pos) (neighbours pos))

    neighbours :: Pos -> [Neighbour]
    neighbours (r, c) = [(NW, lookupState (r-1, c-1)), (N, lookupState (r-1, c)), (NE, lookupState (r-1, c+1)),
                         (W, lookupState (r, c-1)),                               (E, lookupState (r, c+1)),
                         (SW, lookupState (r+1, c-1)), (S, lookupState (r+1, c)), (SE, lookupState (r+1, c+1))]

    -- Función para obtener el estado de una celda dada (usando la tabla original).
    lookupState :: Pos -> State
    lookupState pos = fromMaybe ' ' (lookup pos tab)

execStep :: MonadAC m => m ()
execStep = do tab <- getTable
              e <- getEpoch
              rows <- getRows
              cols <- getCols
              (_, f) <- getGame
              let newTable = evalStep f tab rows cols
              modify (\s -> if newTable == tab
                            then s {isHalted = True}
                            else s {epoch = e + 1, table = newTable})
