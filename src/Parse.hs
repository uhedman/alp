{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module Parse (runP, P, parse) where

import Prelude hiding ( const )
import Text.Parsec hiding (runP,parse)
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Language
    ( emptyDef,
      GenLanguageDef(identLetter, reservedNames),
      LanguageDef )
import Lang (Movimiento (..), Pieza (..), Casilla, Jaque (..))

type P = Parsec String ()

-----------------------
-- Lexer
-----------------------
-- | Analizador de Tokens
lexer :: Tok.TokenParser u
lexer = Tok.makeTokenParser langDef

langDef :: LanguageDef u
langDef = emptyDef {
         reservedNames = ["+", "#", "-"]
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
           case readPieza l of
             Just p -> return p
             Nothing -> fail $ "Pieza no reconocida: " ++ [l]

casilla :: P Casilla
casilla = do l <- letter
             n <- digit
             return (l, read [n])

captura :: P Bool
captura = (char 'x' >> return True) <|> return False

jaque :: P Jaque
jaque = (reserved "+" >> return J) 
    <|> (reserved "#" >> return JM)
    <|> return SJ

normal :: P Movimiento
normal = do p <- try pieza <|> return P
            c <- captura
            c1 <- casilla
            c' <- captura
            c2 <- (casilla >>= \cf -> return (Just cf)) <|> return Nothing
            j <- jaque
            case c2 of
              Nothing -> return $ Normal p Nothing c1 (c || c') j
              Just cf -> return $ Normal p (Just c1) cf (c || c') j

enroque :: P Movimiento
enroque = try (do string "O-O-O"
                  j <- jaque
                  return $ EnroqueLargo j)
          <|>  do string "O-O"
                  j <- jaque
                  return $ EnroqueCorto j

movimiento :: P Movimiento
movimiento = try normal <|> enroque

-- Corre un parser, chequeando que se pueda consumir toda la entrada
runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (whiteSpace *> p <* eof) () filename s

parse :: String -> Either ParseError Movimiento
parse s = runP movimiento s ""

-----------------------
-- Funciones auxiliares
-----------------------

readPieza :: Char -> Maybe Pieza
readPieza 'A' = Just A
readPieza 'C' = Just C
readPieza 'D' = Just D
readPieza 'P' = Just Lang.P
readPieza 'R' = Just R
readPieza 'T' = Just T
readPieza _   = Nothing