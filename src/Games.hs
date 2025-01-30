{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Games where
import Lang (Game, Orientation (..), State, Neighbour)

-- AC para game of life
gameOfLife :: Game
gameOfLife = (['█'], f, "Conway's Game of Life")
  where f ' ' n = if count '█' n == 3 then '█' else ' '
        f '█' n = let ln = count '█' n
                  in if ln == 2 || ln == 3 then '█' else ' '

-- AC para Brian's Brain
briansBrain :: Game
briansBrain = (['█', '·'], f, "Brian's Brain")
  where f ' ' n | count '█' n == 2 = '█'
                | otherwise = ' '
        f '█' _ = '·'
        f '·' _ = ' '

-- AC para game of life
dayAndNight :: Game
dayAndNight = (['█'], f, "Day & Night")
  where f ' ' n = let ln = count '█' n
                  in if ln == 3 || ln == 6 || ln == 7 || ln == 8 then '█' else ' '
        f '█' n = let ln = count '█' n
                  in if ln == 3 || ln == 4 || ln == 6 || ln == 7 || ln == 8 then '█' else ' '

-- AC para la hormiga de langton
langton :: Game
langton = (['█', '^', '>', 'v', '<', 'N', 'E', 'S', 'W'], f, "Langton's Ant")
  where f ' ' n | (N, '>') `elem` n || (N, 'W') `elem` n = 'v'
                | (E, 'v') `elem` n || (E, 'N') `elem` n = '<'
                | (S, '<') `elem` n || (S, 'E') `elem` n = '^'
                | (W, '^') `elem` n || (W, 'S') `elem` n = '>'
                | otherwise = ' '
        f '█' n | (N, '>') `elem` n || (N, 'W') `elem` n = 'S'
                | (E, 'v') `elem` n || (E, 'N') `elem` n = 'W'
                | (S, '<') `elem` n || (S, 'E') `elem` n = 'N'
                | (W, '^') `elem` n || (W, 'S') `elem` n = 'E'
                | otherwise = '█'
        f '^' _ = '█'
        f '>' _ = '█'
        f 'v' _ = '█'
        f '<' _ = '█'
        f 'N' _ = ' '
        f 'E' _ = ' '
        f 'S' _ = ' '
        f 'W' _ = ' '

-- AC para el automata de una dimension a lo largo del tiempo
rule18 :: Game
rule18 = (['█'], f, "Regla 18")
  where f ' ' n | (NW, '█') `elem` n && (N, '█') `elem` n && (NE, '█') `elem` n = ' '
                | (NW, '█') `elem` n && (N, '█') `elem` n && (NE, ' ') `elem` n = ' '
                | (NW, '█') `elem` n && (N, ' ') `elem` n && (NE, '█') `elem` n = ' '
                | (NW, '█') `elem` n && (N, ' ') `elem` n && (NE, ' ') `elem` n = '█'
                | (NW, ' ') `elem` n && (N, '█') `elem` n && (NE, '█') `elem` n = ' '
                | (NW, ' ') `elem` n && (N, '█') `elem` n && (NE, ' ') `elem` n = ' '
                | (NW, ' ') `elem` n && (N, ' ') `elem` n && (NE, '█') `elem` n = '█'
                | (NW, ' ') `elem` n && (N, ' ') `elem` n && (NE, ' ') `elem` n = ' '
                | otherwise = ' '
        f '█' _ = '█'

-- AC para el automata de una dimension a lo largo del tiempo
rule30 :: Game
rule30 = (['█'], f, "Regla 30")
  where f ' ' n | (NW, '█') `elem` n && (N, '█') `elem` n && (NE, '█') `elem` n = ' '
                | (NW, '█') `elem` n && (N, '█') `elem` n && (NE, ' ') `elem` n = ' '
                | (NW, '█') `elem` n && (N, ' ') `elem` n && (NE, '█') `elem` n = ' '
                | (NW, '█') `elem` n && (N, ' ') `elem` n && (NE, ' ') `elem` n = '█'
                | (NW, ' ') `elem` n && (N, '█') `elem` n && (NE, '█') `elem` n = '█'
                | (NW, ' ') `elem` n && (N, '█') `elem` n && (NE, ' ') `elem` n = '█'
                | (NW, ' ') `elem` n && (N, ' ') `elem` n && (NE, '█') `elem` n = '█'
                | (NW, ' ') `elem` n && (N, ' ') `elem` n && (NE, ' ') `elem` n = ' '
                | otherwise = ' '
        f '█' _ = '█'

-- AC para seeds
seeds :: Game
seeds = (['█'], f, "Seeds")
  where f ' ' n | count '█' n == 2 = '█'
                | otherwise = ' '
        f '█' _ = ' '

-- Funciones auxiliares

count :: State -> [Neighbour] -> Int
count c [] = 0
count c ((_,x):xs) | c == x = 1 + count c xs
                   | otherwise = count c xs