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
import MonadAC hiding (State)

-- | Pretty printer para el tablero
t2doc :: Table -> Int -> Int -> Doc AnsiStyle
t2doc table r c =
  let border = '+' : replicate c '-' ++ "+"
      matrix = [[lookup (i,j) table | j <- [1 .. c]] | i <- [1 .. r]]
      separatedList = map (("|" ++) . (++ "|") . concatMap cell2doc) matrix
  in pretty $ unlines $ [border] ++ separatedList ++ [border]

cell2doc :: Maybe State -> String
cell2doc Nothing = " "
cell2doc (Just a) = [a]

-- | Pretty printing del tablero (String)
pp :: MonadAC m => m String
pp = do r <- getRows
        c <- getCols
        tab <- getTable
        return $ render $ t2doc tab r c

render :: Doc AnsiStyle -> String
render = unpack . renderStrict . layoutSmart defaultLayoutOptions
