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
    Atom Bool
  | FVar Char
  | Neg Form
  | Conj Form Form
  | Disy Form Form
  | Cond Form Form
  | Bicond Form Form

-- | AST de lambda terminos
data Term a =
    TVar Var
  | Const Bool
  | Lam Name (Ty a) (Scope a)
  | App (Term a) (Term a)

newtype Scope a = Sc (Term a)

type Name = String

data Var = 
    Bound !Int
  | Free Name
  | Global Name

data Ty a =
    T [a]
  | F ()
  | And (Ty a, Ty a)
  | Or (Either (Ty a) (Ty a))
  | Imp (Ty a -> Ty a)
