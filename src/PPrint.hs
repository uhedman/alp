module PPrint (
  pp
  ) where

import Lang

import Data.Text ( unpack )
import Prettyprinter.Render.Terminal
  ( renderStrict, color, Color (..), AnsiStyle )
import Prettyprinter
  ( annotate,
    defaultLayoutOptions,
    layoutSmart,
    sep,
    Doc,
    Pretty(pretty) )
import MonadChess

--Colores
colorNegras :: Doc AnsiStyle -> Doc AnsiStyle
colorNegras = annotate (color Red)
colorBlancas :: Doc AnsiStyle -> Doc AnsiStyle
colorBlancas = annotate (color Blue)

-- | Pretty printer para el tablero
t2doc :: Tablero -> Doc AnsiStyle
t2doc t = sep (map casilla2doc t)

casilla2doc :: Maybe Pieza -> Doc AnsiStyle
casilla2doc Nothing  = pretty ""
casilla2doc (Just p) = pieza2doc p

pieza2doc :: Pieza -> Doc AnsiStyle
pieza2doc (A B) = colorBlancas (pretty "A")
pieza2doc (A N) = colorNegras (pretty "A")
pieza2doc (C B) = colorBlancas (pretty "C")
pieza2doc (C N) = colorNegras (pretty "C")
pieza2doc (D B) = colorBlancas (pretty "D")
pieza2doc (D N) = colorNegras (pretty "D")
pieza2doc (P B) = colorBlancas (pretty "P")
pieza2doc (P N) = colorNegras (pretty "P")
pieza2doc (R B) = colorBlancas (pretty "R")
pieza2doc (R N) = colorNegras (pretty "R")
pieza2doc (T B) = colorBlancas (pretty "T")
pieza2doc (T N) = colorNegras (pretty "T")

-- | Pretty printing del tablero (String)
pp :: MonadChess m => m String
pp = render . t2doc <$> getTablero

render :: Doc AnsiStyle -> String
render = unpack . renderStrict . layoutSmart defaultLayoutOptions
