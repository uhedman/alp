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
    Doc,
    punctuate,
    Pretty(pretty) )
import MonadChess
import Data.Array (elems)

--Colores
colorNegras :: Doc AnsiStyle -> Doc AnsiStyle
colorNegras = annotate (color Red)
colorBlancas :: Doc AnsiStyle -> Doc AnsiStyle
colorBlancas = annotate (color Blue)

-- | Pretty printer para el tablero
t2doc :: Tablero -> Doc AnsiStyle
t2doc t = 
  let separador = pretty "\n+---+---+---+---+---+---+---+---+\n"
      matrix = [take 8 (drop ((8-i)*8) (elems t)) | i <- [1 .. 8]]
      separatedList = mconcat $ punctuate separador (map fila2doc matrix)
  in mconcat [separador, separatedList, separador]

fila2doc :: [Maybe PiezaJugador] -> Doc AnsiStyle
fila2doc f = 
  let separador = pretty "|"
      separatedList = mconcat $ punctuate separador (map casilla2doc f)
  in mconcat [separador, separatedList, separador]

casilla2doc :: Maybe PiezaJugador -> Doc AnsiStyle
casilla2doc Nothing  = pretty "   "
casilla2doc (Just p) = pieza2doc p

pieza2doc :: PiezaJugador -> Doc AnsiStyle
pieza2doc (A, B) = colorBlancas (pretty " ♗ ")
pieza2doc (A, N) = colorNegras (pretty " ♗ ")
pieza2doc (C, B) = colorBlancas (pretty " ♘ ")
pieza2doc (C, N) = colorNegras (pretty " ♘ ")
pieza2doc (D, B) = colorBlancas (pretty " ♕ ")
pieza2doc (D, N) = colorNegras (pretty " ♕ ")
pieza2doc (P, B) = colorBlancas (pretty " ♙ ")
pieza2doc (P, N) = colorNegras (pretty " ♙ ")
pieza2doc (R, B) = colorBlancas (pretty " ♔ ")
pieza2doc (R, N) = colorNegras (pretty " ♔ ")
pieza2doc (T, B) = colorBlancas (pretty " ♖ ")
pieza2doc (T, N) = colorNegras (pretty " ♖ ")

-- | Pretty printing del tablero (String)
pp :: MonadChess m => m String
pp = render . t2doc <$> getTablero

render :: Doc AnsiStyle -> String
render = unpack . renderStrict . layoutSmart defaultLayoutOptions
