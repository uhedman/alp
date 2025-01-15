module Eval where
import MonadAC
import Lang
import Global
import Data.List

execSet :: MonadAC m => Action -> m ()
execSet action = do
  tab <- getTable
  let (shouldModify, updateFunc) = case action of
        Kill p   -> (p `elem` tab, delete p tab)
        Revive p -> (p `notElem` tab, p : delete p tab)
  when shouldModify $ modify (\s -> s {table = updateFunc, isHalted = False})

neighbs :: Int -> Int -> Pos -> [Pos]
neighbs r c (x, y) = filter inbounds positions
  where
    positions = [(x-1, y-1), (x,   y-1),
                 (x+1, y-1), (x-1, y),
                 (x+1, y),   (x-1, y+1),
                 (x,   y+1), (x+1, y+1)]
    inbounds = precomputedBounds r c

precomputedBounds :: Int -> Int -> Pos -> Bool
precomputedBounds r c (a, b) = a >= 1 && b >= 1 && a <= r && b <= c

filterSurvivors :: Table -> Table
filterSurvivors tab = filter (\p -> countNeighbours tab p == 2 || countNeighbours tab p == 3) tab

calculateBirths :: Table -> Int -> Int -> Table
calculateBirths tab r c = [p | p <- nub (concatMap (neighbs r c) tab),
                               p `notElem` tab,
                               countNeighbours tab p == 3]

evalStep :: Table -> Int -> Int -> Table
evalStep tab r c = filterSurvivors tab ++ calculateBirths tab r c

execStep :: MonadAC m => m ()
execStep = do tab <- getTable
              e <- getEpoch
              c <- getCols
              r <- getRows
              let newTable = evalStep tab r c
              modify (\s -> if newTable == tab
                            then s {isHalted = True}
                            else s {epoch = e + 1, table = newTable})

countNeighbours :: Table -> Pos -> Int
countNeighbours tab (x, y) = length $ filter (`elem` tab) pos
  where pos = [(x-1, y-1), (x, y-1), (x+1, y-1),
               (x-1, y),             (x+1, y),
               (x-1, y+1), (x, y+1), (x+1, y+1)]
