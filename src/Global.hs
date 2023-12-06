module Global where
import Lang
import Data.Array (listArray)

data GlEnv = GlEnv {
  tablero :: Tablero,
  turno :: Jugador,
  enroqueBlanco :: Bool,
  enroqueNegro :: Bool,
  lfile :: String
}

-- | Valor del estado inicial
initialEnvChess :: GlEnv
initialEnvChess = GlEnv tableroInicial B True True ""

initialEnvEmpty :: GlEnv
initialEnvEmpty = GlEnv tableroVacio B True True ""

tableroVacio :: Tablero
tableroVacio = listArray ((1, 1), (8, 8)) [Nothing | i <- [1..64]]

tableroInicial :: Tablero
tableroInicial = listArray ((1, 1), (8, 8))
  [Just (T, B), Just (C, B), Just (A, B), Just (D, B), Just (R, B), Just (A, B), Just (C, B), Just (T, B),
   Just (P, B), Just (P, B), Just (P, B), Just (P, B), Just (P, B), Just (P, B), Just (P, B), Just (P, B),
   Nothing    , Nothing    , Nothing    , Nothing    , Nothing    , Nothing    , Nothing    , Nothing    ,
   Nothing    , Nothing    , Nothing    , Nothing    , Nothing    , Nothing    , Nothing    , Nothing    ,
   Nothing    , Nothing    , Nothing    , Nothing    , Nothing    , Nothing    , Nothing    , Nothing    ,
   Nothing    , Nothing    , Nothing    , Nothing    , Nothing    , Nothing    , Nothing    , Nothing    ,
   Just (P, N), Just (P, N), Just (P, N), Just (P, N), Just (P, N), Just (P, N), Just (P, N), Just (P, N),
   Just (T, N), Just (C, N), Just (A, N), Just (D, N), Just (R, N), Just (A, N), Just (C, N), Just (T, N)]
