module Global where
import Lang

data GlEnv = GlEnv {
  table :: Table,
  cols :: Int,
  rows :: Int,
  epoch :: Int,
  isHalted :: Bool,
  lfile :: String
}

-- | Valor del estado inicial
initialEnvEmpty :: GlEnv
initialEnvEmpty = GlEnv [] 5 5 0 False ""
