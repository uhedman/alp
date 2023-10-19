{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDeriving #-}

{-|
Module      : Lang
Description : AST de términos y tipos
Copyright   : (c) Ulises Hedman, 2023.
License     : GPL-3
Maintainer  : ulyhedman@hotmail.com
Stability   : experimental

Definiciones de distintos tipos de datos:
  - AST de términos
  - Tipos
-}

module Lang where

-- | AST de formulas
data Form  =
    FConst Bool
  | FImp Form Form
  | FConj Form Form
  | FDisy Form Form

-- | AST de lambda terminos
data Term a =
    Var Name (Ty a)
  | Const Bool
  | Lam Name (Ty a) (Term a)
  | App (Term a) (Term a)

type Name = String

data Ty a =
    T [a]
  | F ()
  | And (Ty a, Ty a)
  | Or (Either (Ty a) (Ty a))
  | Imp (Ty a -> Ty a)
