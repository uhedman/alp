module Eval where
import MonadAC
import Lang
import Global
import Data.List

execSet :: MonadAC m => Action -> m ()
execSet (Kill p) = do tab <- getTable
                      when (p `elem` tab) $ do let newTable = delete p tab
                                               modify (\s -> s {table = newTable, isHalted = False})
execSet (Revive p) = do tab <- getTable
                        unless (p `elem` tab) $ do let newTable = p : delete p tab
                                                   modify (\s -> s {table = newTable, isHalted = False})

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

neighbs :: Int -> Int -> Pos -> [Pos]
neighbs r c (x, y) = filter inbounds [(x-1, y-1), (x,   y-1),
                                      (x+1, y-1), (x-1, y),
                                      (x+1, y),   (x-1, y+1),
                                      (x,   y+1), (x-1, y+1)]
  where inbounds (a, b) = a >= 1 && b >= 1 && a <= r && b <= c
  

step :: Table -> Int -> Int -> Table
step tab c r = survivors ++ births
  where survivors = filter (\p -> countNeighbours tab p == 2 || countNeighbours tab p == 3) tab
        births = [p | p <- rmdups (concatMap (neighbs c r) tab),
                      p `notElem` tab,
                      countNeighbours tab p == 3]

execStep :: MonadAC m => m ()
execStep = do tab <- getTable
              e <- getEpoch
              c <- getCols
              r <- getRows
              let newTable = step tab c r
              modify (\s -> if newTable == tab
                            then s {isHalted = True}
                            else s {epoch = e + 1, table = newTable})

countNeighbours :: Table -> Pos -> Int
countNeighbours tab (x, y) = length $ filter (`elem` tab) pos
  where pos = [(x-1, y-1), (x, y-1), (x+1, y-1),
               (x-1, y),             (x+1, y),
               (x-1, y+1), (x, y+1), (x+1, y+1)]
