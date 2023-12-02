module Eval where
import MonadChess (MonadChess, modify, getEnroqueBlanco, getEnroqueNegro, getTurno, getTablero)
import Lang (Movimiento (..), Jugador (..), Tablero, Pieza (..), PiezaJugador, Casilla)
import Global ( GlEnv(tablero, turno) )
import Data.Array ( (//) )

execMove :: MonadChess m => Movimiento -> m ()
execMove mv = do player <- getTurno
                 tab <- getTablero
                 (b, nuevoTablero) <- aux mv player tab
                 modify (\s -> s {turno = change player, tablero = nuevoTablero})

aux :: MonadChess m => Movimiento -> Jugador -> Tablero -> m (Bool, Tablero)
aux (Normal p ci cf x j) player tab = return (True, tab)
aux (EnroqueCorto j) B tab =
   do b <- getEnroqueBlanco 
      if not b 
      then return (False, tab)
      else do let tab' = mover (1,5) (1,7) (R,B) tab
              let tab'' = mover (1,8) (1,6) (T,B) tab'
              return (True, tab'') 
aux (EnroqueCorto j) N tab =
  do b <- getEnroqueNegro 
     if not b 
     then return (False, tab)
     else do let tab' = mover (8,5) (8,7) (R,N) tab
             let tab'' = mover (8,8) (8,6) (T,N) tab'
             return (True, tab'') 
aux (EnroqueLargo j) B tab =
  do b <- getEnroqueBlanco 
     if not b
     then return (False, tab)
     else do let tab' = mover (1,5) (1,3) (R,B) tab
             let tab'' = mover (1,1) (1,4) (T,B) tab'
             return (True, tab'') 
aux (EnroqueLargo j) N tab =
  do b <- getEnroqueNegro 
     if not b
     then return (False, tab)
     else do let tab' = mover (8,5) (8,3) (R,N) tab
             let tab'' = mover (8,1) (8,4) (T,N) tab'
             return (True, tab'') 

mover :: Casilla -> Casilla -> PiezaJugador -> Tablero -> Tablero
mover ci cf p tab = tab // [(ci, Nothing), (cf, Just p)]

change :: Jugador -> Jugador
change B = N
change N = B