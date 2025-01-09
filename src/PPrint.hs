module PPrint (
  pp
  ) where

import Lang

import Data.Text ( unpack )
import Prettyprinter.Render.Terminal
  ( renderStrict, AnsiStyle )
import Prettyprinter
  ( defaultLayoutOptions,
    layoutSmart,
    Doc,
    Pretty(pretty) )
import MonadAC

-- | Pretty printer para el tablero
t2doc :: Table -> Int -> Int -> Doc AnsiStyle
t2doc table r c =
  let border = replicate (c + 2) '+'
      matrix = [[(i, j) `elem` table | i <- [1 .. c]] | j <- [1 .. r]]
      separatedList = map (\row -> "+" ++ concatMap cell2doc row ++ "+") matrix
  in pretty $ unlines $ [border] ++ separatedList ++ [border]

cell2doc :: Bool -> String
cell2doc True = "#"
cell2doc False = " "

-- | Pretty printing del tablero (String)
pp :: MonadAC m => m String
pp = do r <- getRows
        c <- getCols
        tab <- getTable
        return $ render $ t2doc tab r c

render :: Doc AnsiStyle -> String
render = unpack . renderStrict . layoutSmart defaultLayoutOptions
