{-|
Module      : Eval
Description : Evalúa un término siguiendo la semántica big-step
Copyright   : (c) Ulises Hedman 2023.
License     : GPL-3
Maintainer  : ulyhedman@hotmail.com
Stability   : experimental

Este módulo evaluá términos siguiendo la semántica big-step (estrategia CBV)
-}

module Eval where

import Common ( abort )
import Lang
import Subst ( subst )
import MonadFD4 ( MonadFD4 )

-- | Evaluador de términos CBV
eval ::  MonadFD4 m => Term a -> m (Term a)
eval (App l r) = do
     le <- eval l
     re <- eval r
     case (le, re) of
        (Lam y _ m, n) ->
           eval (subst n m)
        _ ->
           abort "Error de tipo en runtime "

-- nada más para reducir
eval t = return t
