{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Games where
import Lang (Game, Orientation (..), State, Neighbour)

-- AC para game of life
gameOfLife :: Game
gameOfLife = ([' ','#'], f)
  where f ' ' n = if count '#' n == 3 then '#' else ' '
        f '#' n = let ln = count '#' n
                  in if ln == 2 || ln == 3 then '#' else ' '

-- AC para la hormiga de langton
langton :: Game
langton = ([' ', '#', '^', '>', 'v', '<', 'N', 'E', 'S', 'W'], f)
  where f ' ' n | (N, '>') `elem` n || (N, 'W') `elem` n = 'v'
                | (E, 'v') `elem` n || (E, 'N') `elem` n = '<'
                | (S, '<') `elem` n || (S, 'E') `elem` n = '^'
                | (W, '^') `elem` n || (W, 'S') `elem` n = '>'
                | otherwise = ' '
        f '#' n | (N, '>') `elem` n || (N, 'W') `elem` n = 'S'
                | (E, 'v') `elem` n || (E, 'N') `elem` n = 'W'
                | (S, '<') `elem` n || (S, 'E') `elem` n = 'N'
                | (W, '^') `elem` n || (W, 'S') `elem` n = 'E'
                | otherwise = '#'
        f '^' _ = '#'
        f '>' _ = '#'
        f 'v' _ = '#'
        f '<' _ = '#'
        f 'N' _ = ' '
        f 'E' _ = ' '
        f 'S' _ = ' '
        f 'W' _ = ' '

-- AC para el automata de una dimension a lo largo del tiempo
oneDimension :: Game
oneDimension = ([' ', '#'], f)
  where f ' ' n | (NW, '#') `elem` n && (N, '#') `elem` n && (NE, '#') `elem` n = ' '
                | (NW, '#') `elem` n && (N, '#') `elem` n && (NE, ' ') `elem` n = ' '
                | (NW, '#') `elem` n && (N, ' ') `elem` n && (NE, '#') `elem` n = ' '
                | (NW, '#') `elem` n && (N, ' ') `elem` n && (NE, ' ') `elem` n = '#'
                | (NW, ' ') `elem` n && (N, '#') `elem` n && (NE, '#') `elem` n = ' '
                | (NW, ' ') `elem` n && (N, '#') `elem` n && (NE, ' ') `elem` n = ' '
                | (NW, ' ') `elem` n && (N, ' ') `elem` n && (NE, '#') `elem` n = '#'
                | (NW, ' ') `elem` n && (N, ' ') `elem` n && (NE, ' ') `elem` n = ' '
                | otherwise = ' '
        f '#' _ = '#'

-- Funciones auxiliares

count :: State -> [Neighbour] -> Int
count c [] = 0
count c ((_,x):xs) | c == x = 1 + count c xs
                   | otherwise = count c xs