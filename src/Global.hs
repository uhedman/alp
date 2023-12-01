module Global where
import Lang

data GlEnv = GlEnv {
  inter :: Bool,
  tablero :: Tablero,
  turno :: Jugador,
  lfile :: String
}

-- | Valor del estado inicial
initialEnv :: GlEnv
initialEnv = GlEnv False tableroInicial B ""

tableroInicial :: [Maybe Pieza]
tableroInicial = 
  [Just (T N), Just (C N), Just (A N), Just (D N), Just (R N), Just (A N), Just (C N), Just (T N),
   Just (P N), Just (P N), Just (P N), Just (P N), Just (P N), Just (P N), Just (P N), Just (P N),
   Nothing   , Nothing   , Nothing   , Nothing   , Nothing   , Nothing   , Nothing   , Nothing   ,
   Nothing   , Nothing   , Nothing   , Nothing   , Nothing   , Nothing   , Nothing   , Nothing   ,
   Nothing   , Nothing   , Nothing   , Nothing   , Nothing   , Nothing   , Nothing   , Nothing   ,
   Nothing   , Nothing   , Nothing   , Nothing   , Nothing   , Nothing   , Nothing   , Nothing   ,
   Nothing   , Nothing   , Nothing   , Nothing   , Nothing   , Nothing   , Nothing   , Nothing   ,
   Nothing   , Nothing   , Nothing   , Nothing   , Nothing   , Nothing   , Nothing   , Nothing   ,
   Just (P B), Just (P B), Just (P B), Just (P B), Just (P B), Just (P B), Just (P B), Just (P B),
   Just (T B), Just (C B), Just (A B), Just (D B), Just (R B), Just (A B), Just (C B), Just (T B)]
