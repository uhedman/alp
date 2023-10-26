{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use record patterns" #-}
{-|
Module      : PPrint
Description : Pretty printer para proposiciones y terminos.
Copyright   : (c) Ulises Hedman, 2023.
License     : -3
Maintainer  : ulyhedman@hotmail.com
Stability   : experimental

-}

module PPrint (
    pp,
    ppTy,
    ppName,
    ) where

import Lang

import Data.Text ( unpack )
import Prettyprinter.Render.Terminal
  ( renderStrict, italicized, color, colorDull, Color (..), AnsiStyle )
import Prettyprinter
    ( (<+>),
      annotate,
      defaultLayoutOptions,
      layoutSmart,
      nest,
      sep,
      parens,
      Doc,
      Pretty(pretty) )
import MonadFD4 ( MonadFD4 )

freshen :: [Name] -> Name -> Name
freshen ns n = let cands = n : map (\i -> n ++ show i) [0..] 
               in head (filter (`notElem` ns) cands)

--Colores
constColor :: Doc AnsiStyle -> Doc AnsiStyle
constColor = annotate (color Red)
opColor :: Doc AnsiStyle -> Doc AnsiStyle
opColor = annotate (colorDull Green)
keywordColor :: Doc AnsiStyle -> Doc AnsiStyle
keywordColor = annotate (colorDull Green) -- <> bold)
typeColor :: Doc AnsiStyle -> Doc AnsiStyle
typeColor = annotate (color Blue <> italicized)
typeOpColor :: Doc AnsiStyle -> Doc AnsiStyle
typeOpColor = annotate (colorDull Blue)
defColor :: Doc AnsiStyle -> Doc AnsiStyle
defColor = annotate (colorDull Magenta <> italicized)
nameColor :: Doc AnsiStyle -> Doc AnsiStyle
nameColor = id

-- | Pretty printer de nombres (Doc)
name2doc :: Name -> Doc AnsiStyle
name2doc = nameColor . pretty

-- |  Pretty printer de nombres (String)
ppName :: Name -> String
ppName = id

-- | Pretty printer para tipos (Doc)
ty2doc :: Ty a -> Doc AnsiStyle
ty2doc (T _) = typeColor (pretty "T")
ty2doc (F _) = typeColor (pretty "F")
ty2doc (And _) = typeColor (pretty "&")
ty2doc (Or _) = typeColor (pretty "|")
ty2doc (Imp _) = typeColor (pretty "->")

c2doc :: Bool -> Doc AnsiStyle
c2doc True = constColor (pretty "T")
c2doc False = constColor (pretty "F")

-- | Pretty printer para tipos (String)
ppTy :: Ty a -> String
ppTy = render . ty2doc

collectApp :: Term a -> (Term a, [Term a])
collectApp = go [] where
  go ts (App h tt) = go (tt:ts) h
  go ts h = (h, ts)

parenIf :: Bool -> Doc a -> Doc a
parenIf True = parens
parenIf _ = id

-- t2doc at t :: Doc
-- at: debe ser un átomo
-- | Pretty printing de términos (Doc)
t2doc :: Bool     -- Debe ser un átomo? 
      -> Term a   -- término a mostrar
      -> Doc AnsiStyle
-- Uncomment to use the Show instance for STerm
{- t2doc at x = text (show x) -}
t2doc at (TVar (Bound i)) = pretty (show i)
t2doc at (TVar (Free x)) = name2doc x
t2doc at (TVar (Global x)) = name2doc x 
t2doc at (Const c) = c2doc c
t2doc at (Lam x ty (Sc t)) =
  parenIf at $
  sep [sep [ keywordColor (pretty "fun")
           , name2doc x
           , opColor(pretty ":")
           , ty2doc ty
           , opColor(pretty "->")]
      , nest 2 (t2doc False t)]

t2doc at t@(App _ _) =
  let (h, ts) = collectApp t in
  parenIf at $
  t2doc True h <+> sep (map (t2doc True) ts)

-- | Pretty printing de términos (String)
pp :: MonadFD4 m => Term a -> m String
-- Uncomment to use the Show instance for Term
{- pp = show -}
pp t = do
       return (render . t2doc False $ t)

render :: Doc AnsiStyle -> String
render = unpack . renderStrict . layoutSmart defaultLayoutOptions
