module Global where
import Lang

data GlEnv = GlEnv {
  table :: Table,
  cols :: Int,
  rows :: Int,
  epoch :: Int,
  isHalted :: Bool
}

-- | Valor del estado inicial
initialEnv :: (Table, Int, Int) -> GlEnv
initialEnv (t, nRows, nCols) = GlEnv t nRows nCols 0 False

emptyTable :: (Table, Int, Int)
emptyTable = ([], 6, 6)