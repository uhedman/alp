{-|
Module      : Global
Description : Define el estado global del demostrador de teoremas
Copyright   : (c) Ulises Hedman, 2023.
License     : GPL-3
Maintainer  : ulyhedman@hotmail.com
Stability   : experimental
-}

module Global where

data GlEnv = GlEnv {
  inter :: Bool,        --  ^ True, si estamos en modo interactivo.
                        -- Este parámetro puede cambiar durante la ejecución:
                        -- Es falso mientras se cargan archivos, pero luego puede ser verdadero.
  lfile :: String       -- ^ Último archivo cargado.
}

-- | Valor del estado inicial
initialEnv :: GlEnv
initialEnv = GlEnv False ""
