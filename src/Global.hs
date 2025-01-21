module Global where
import Lang ( Table, Game )
import Games ( oneDimension )

data GlEnv = GlEnv {
  game :: Game,
  table :: Table,
  rows :: Int,
  cols :: Int,
  epoch :: Int,
  isHalted :: Bool
}

-- | Valor del estado inicial
initialEnv :: (Table, Int, Int) -> GlEnv
initialEnv (t, nRows, nCols) = GlEnv oneDimension t nRows nCols 0 False

emptyTable :: (Table, Int, Int)
emptyTable = ([], 6, 6)