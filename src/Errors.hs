module Errors where

import Text.Parsec.Error ( ParseError )

-- Agregar más, y source positions
newtype Error = ParseErr ParseError

instance Show Error where
  show (ParseErr e) = show e
