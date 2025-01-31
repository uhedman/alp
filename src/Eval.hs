module Eval ( execSet, execStep ) where
import MonadAC
    ( getCols, getEpoch, getRows, getTable, getGame, modify, MonadAC, getBoundary )
import Lang ( Pos, Table, State, Neighbour, Orientation (..), Boundary (..) )
import Global ( GlEnv(table, isHalted, epoch) )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

execSet :: MonadAC m => (Pos, State) -> m ()
execSet (pos, state) = do
  r <- getRows
  c <- getCols
  do tab <- getTable
     let newEntry = (pos, state)
         updatedTable = Map.insert pos state tab
     modify (\s -> s {table = updatedTable, isHalted = False})
  
execStep :: MonadAC m => m ()
execStep = do 
  tab <- getTable
  e <- getEpoch
  rows <- getRows
  cols <- getCols
  (_, f, _) <- getGame
  bound <- getBoundary
  let newTable = evalStep f bound tab rows cols
  modify (\s -> if newTable == tab
                then s {isHalted = True}
                else s {epoch = e + 1, table = newTable})

evalStep :: (State -> [Neighbour] -> State) -> Boundary -> Table -> Int -> Int -> Table
evalStep f bound tab rows cols = 
    foldr (\pos acc -> let value = f (lookupState pos) (neighboursWithStates pos)
                       in if value == ' ' 
                          then acc
                          else Map.insert pos value acc) Map.empty cells
  where
    cells :: Set.Set Pos
    cells = foldMap (Set.fromList . neighboursPositions) (Map.keys tab)

    neighboursPositions :: Pos -> [Pos]
    neighboursPositions (r, c) = case bound of
          Closed -> filter inbounds positions
          Open -> positions
          Periodic -> map periodic positions
          Reflective -> filter inbounds positions
      where
      positions = [(r-1, c-1), (r-1, c), (r-1, c+1),
                   (r, c-1),   (r, c),   (r, c+1),
                   (r+1, c-1), (r+1, c), (r+1, c+1)]
      
    neighboursWithStates :: Pos -> [Neighbour]
    neighboursWithStates (r, c) = case bound of
        Closed -> map (\(dir, pos) -> (dir, lookupState pos)) closedNeighbours
        Open -> map (\(dir, pos) -> (dir, lookupState pos)) openNeighbours
        Periodic -> map (\(dir, pos) -> (dir, lookupState (periodic pos))) openNeighbours
        Reflective -> map (\(dir, pos) -> (dir, lookupState (reflect pos))) openNeighbours
      where
        openNeighbours =
          [(NW, (r-1, c-1)), (N, (r-1, c)), (NE, (r-1, c+1)),
           (W, (r, c-1)),                   (E, (r, c+1)),
           (SW, (r+1, c-1)), (S, (r+1, c)), (SE, (r+1, c+1))]
        
        closedNeighbours = filter (inbounds . snd) openNeighbours

    lookupState :: Pos -> State
    lookupState pos = Map.findWithDefault ' '  pos tab
    
    inbounds :: Pos -> Bool
    inbounds = isInBound rows cols
    
    periodic :: Pos -> Pos
    periodic (x, y) = let x' = ((x - 1) `mod` rows) + 1
                          y' = ((y - 1) `mod` cols) + 1
                      in (x', y')

    reflect :: Pos -> Pos
    reflect (x, y) = (min rows (max 1 x), min cols (max 1 y))

isInBound :: Int -> Int -> Pos -> Bool
isInBound rows cols (a, b) = a >= 1 && b >= 1 && a <= rows && b <= cols