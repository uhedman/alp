{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-|
Module      : Parse
Description : Define un parser de formulas.
Copyright   : (c) Us Hedman, 2023.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

-}

module Parse (Parse.parse, runP, P, form) where

import Prelude hiding ( const )
import Lang hiding (getPos)
import Text.Parsec hiding (runP,parse)
--import Data.Char ( isNumber, ord )
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Language --( GenLanguageDef(..), emptyDef )

type P = Parsec String ()

-----------------------
-- Lexer
-----------------------
-- | Analizador de Tokens
lexer :: Tok.TokenParser u
lexer = Tok.makeTokenParser langDef

langDef :: LanguageDef u
langDef = emptyDef {
         commentLine    = "#",
         reservedNames = ["True", "False"],
         reservedOpNames = ["<->","->","|","&","¬"]
        }

whiteSpace :: P ()
whiteSpace = Tok.whiteSpace lexer

natural :: P Integer 
natural = Tok.natural lexer

stringLiteral :: P String
stringLiteral = Tok.stringLiteral lexer

parens :: P a -> P a
parens = Tok.parens lexer

identifier :: P String
identifier = Tok.identifier lexer

reserved :: String -> P ()
reserved = Tok.reserved lexer

reservedOp :: String -> P ()
reservedOp = Tok.reservedOp lexer

tyIdentifier :: P String
tyIdentifier = Tok.lexeme lexer $ do
  c  <- upper
  cs <- many (identLetter langDef)
  return (c:cs)

-----------------------
-- Parser
-----------------------

form1 :: P Form
form1 = do{ x <- reserved "True"
          ; return (Atom True)
          }
    <|> do{ x <- reserved "False"
          ; return (Atom False)
          }
    <|> parens form

form2 :: P (Form -> Form -> Form)
form2 = do{ reservedOp "|"
          ; return Conj
          }
    <|> do{ reservedOp "&"
          ; return Disy
          }
    <|> do{ reservedOp "->"
          ; return Cond
          }
    <|> do{ reservedOp "<->"
          ; return Cond
          }

form :: P Form
form = chainl1 form1 form2

-- Corre un parser, chequeando que se pueda consumir toda la entrada
runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (whiteSpace *> p <* eof) () filename s

--para debugging en uso interactivo (ghci)
parse :: String -> Form
parse s = case runP form s "" of
            Right t -> t
            Left e -> error ("no parse: " ++ show s)
