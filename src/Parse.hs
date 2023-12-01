{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module Parse (runP, P) where

import Prelude hiding ( const )
import Text.Parsec hiding (runP,parse)
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Language
    ( emptyDef,
      GenLanguageDef(identLetter, reservedNames),
      LanguageDef )
import Lang (Movimiento (Movimiento), Pieza, Casilla, Jaque (M, J, Nada))

type P = Parsec String ()

-----------------------
-- Lexer
-----------------------
-- | Analizador de Tokens
lexer :: Tok.TokenParser u
lexer = Tok.makeTokenParser langDef

langDef :: LanguageDef u
langDef = emptyDef {
         reservedNames = ["x", "+", "#", "-"]
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

pieza :: P Pieza
pieza = do l <- letter
           return (read [l])

casilla :: P Casilla
casilla = do l <- letter
             n <- digit
             return (l, read [n])

captura :: P Bool
captura = try (do reserved "x" >> return True)
            <|> return False

jaque :: P Jaque
jaque = try (do reserved "+" >> return J)
          <|> try (do reserved "+" >> return M)
            <|> return Nada

movimiento :: P Movimiento
movimiento = do p <- pieza
                i <- casilla
                b <- captura
                f <- casilla
                j <- jaque
                return $ Movimiento p i f b j

-- Corre un parser, chequeando que se pueda consumir toda la entrada
runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (whiteSpace *> p <* eof) () filename s

-- para debugging en uso interactivo (ghci)
-- parse :: String -> Form
-- parse s = case runP form s "" of
--             Right t -> t
--             Left e -> error ("no parse: " ++ show s)
