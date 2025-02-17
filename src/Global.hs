module Global where
import Lang ( Table, Game, Boundary (Open) )
import Games ( gameOfLife )
import Data.Maybe (fromMaybe)
import Data.Map.Strict as Map

data GlEnv = GlEnv {
  game :: Game,
  boundary :: Boundary,
  table :: Table,
  rows :: Int,
  cols :: Int,
  epoch :: Int,
  isHalted :: Bool,
  lastFile :: FilePath
}

-- | Valor del estado inicial
initialEnv :: (Maybe FilePath, Maybe Game, Maybe Boundary) -> GlEnv
initialEnv (f, g, b) = GlEnv (fromMaybe gameOfLife g) (fromMaybe Open b) Map.empty 6 6 0 False (fromMaybe "" f)