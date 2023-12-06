module Eval where
import MonadChess (MonadChess, modify, getEnroqueBlanco, getEnroqueNegro, getTurno, getTablero)
import Lang (Movimiento (..), Jugador (..), Tablero, Pieza (..), PiezaJugador, Casilla)
import Global ( GlEnv(tablero, turno) )
import Data.Array ( (//), elems )
import Data.List (elemIndex)

execPlace :: MonadChess m => Pieza -> Jugador -> Casilla -> m ()
execPlace p j c = do tab <- getTablero
                     let nuevoTablero = tab // [(c, Just (p,j))]
                     modify (\s -> s {tablero = nuevoTablero})

execMove :: MonadChess m => Movimiento -> m ()
execMove mv = do player <- getTurno
                 tab <- getTablero
                 nuevoTablero <- aux mv player tab
                 case nuevoTablero of
                   Nothing -> return ()
                   Just t -> modify (\s -> s {turno = change player, tablero = t})

aux :: MonadChess m => Movimiento -> Jugador -> Tablero -> m (Maybe Tablero)
aux (Normal p Nothing cf _ _) player tab = 
  if p == P 
  then case search P cf player tab of
         Just ci -> return $ Just $ mover ci cf (p, player) tab
         Nothing -> return Nothing
  else do let Just ci = elemIndex (Just (p, player)) (elems tab)
          return $ Just $ mover (div ci 8 + 1, mod ci 8 + 1) cf (p, player) tab
aux (Normal p (Just ci) cf _ _) player tab = 
  return $ Just $ mover ci cf (p, player) tab
aux (EnroqueCorto j) B tab =
  do b <- getEnroqueBlanco 
     if not b 
     then return Nothing
     else do let tab' = mover (1,5) (1,7) (R,B) tab
             let tab'' = mover (1,8) (1,6) (T,B) tab'
             return $ Just tab''
aux (EnroqueCorto j) N tab =
  do b <- getEnroqueNegro 
     if not b 
     then return Nothing
     else do let tab' = mover (8,5) (8,7) (R,N) tab
             let tab'' = mover (8,8) (8,6) (T,N) tab'
             return $ Just tab'' 
aux (EnroqueLargo j) B tab =
  do b <- getEnroqueBlanco 
     if not b
     then return Nothing
     else do let tab' = mover (1,5) (1,3) (R,B) tab
             let tab'' = mover (1,1) (1,4) (T,B) tab'
             return $ Just tab'' 
aux (EnroqueLargo j) N tab =
  do b <- getEnroqueNegro 
     if not b
     then return Nothing
     else do let tab' = mover (8,5) (8,3) (R,N) tab
             let tab'' = mover (8,1) (8,4) (T,N) tab'
             return $ Just tab'' 

mover :: Casilla -> Casilla -> PiezaJugador -> Tablero -> Tablero
mover ci cf p tab = tab // [(ci, Nothing), (cf, Just p)]

change :: Jugador -> Jugador
change B = N
change N = B

search :: Pieza -> Casilla -> Jugador -> Tablero -> Maybe Casilla
search P cf player tab = Nothing