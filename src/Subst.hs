{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE GADTs #-}
{-|
Module      : Subst
Description : Define las operaciones de la representacion locally nameless
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

Este módulo define las operaciones de la representacion locally nameless,
y la substitución.

-}

module Subst where

import Lang
import Common

-- Esta es una función auxiliar que usan el resto de las funciones de este módulo
-- para modificar las vsriables (ligadas y libres) de un término
varChanger :: (Int -> Name -> Term a) --que hacemos con las variables localmente libres
           -> (Int -> Int ->  Term a) --que hacemos con los indices de De Bruijn
           -> Term a -> Term a
varChanger local bound t = go 0 t where
  go n (TVar (Bound i))  = bound n i
  go n (TVar (Free x))   = local n x 
  go n (TVar (Global x)) = TVar (Global x) 
  go n (Lam y ty (Sc t)) = Lam y ty (Sc (go (n+1) t))
  go n (App l r)         = App (go n l) (go n r)
  go n t@(Const _)       = t

-- `open n t` reemplaza la primera variable ligada
-- en `t` (que debe ser un Scope con una sola variable que 
-- escapa al término) por el nombre libre `n`.
-- La variable Bound 0 pasa a ser Free n. El nombre `n`
-- debe ser fresco en el término para que no ocurra shadowing.
open :: Name -> Scope a -> Term a
open nm (Sc t) = varChanger (\_ n -> TVar (Free n)) bnd t
   where bnd depth i | i <  depth = TVar (Bound i)
                     | i == depth =  TVar (Free nm)
                     | otherwise  = abort "open: M is not LC"

-- `subst u t` sustituye el índice de de Bruijn 0 en t con
-- el término u (Bound 0 pasa a ser u). Notar que t es un Scope 
-- con un solo índice que escapa el término.
-- Puede pensarse como una optimizacíon de primero hacer `open
-- n t`, con nombres frescos, y luego sustituir los nombres
-- por los términos correspondientes. La ventaja es que no hace falta
-- generar ningún nombre, y por lo tanto evitamos la necesidad de
-- nombres frescos.
subst :: Term a -> Scope a -> Term a
subst n (Sc m) = varChanger (\_ n -> TVar (Free n)) bnd m
  where bnd depth i | i <  depth = TVar (Bound i)
                    | i == depth = n
                    | otherwise  = abort "substN: M is not LC"

-- `close n t` es la operación inversa a open. Reemplaza
-- las variables `Free n` por la variable ligada `Bound 0`.
close :: Name -> Term a -> Scope a
close nm t = Sc (varChanger lcl (\_ i -> TVar (Bound i)) t)
  where lcl depth y = if y==nm 
                      then TVar (Bound depth)
                      else TVar (Free y)
