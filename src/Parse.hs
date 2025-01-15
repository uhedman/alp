{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE TupleSections #-}

module Parse (runP, P, parseSet, program) where

import Prelude hiding ( const )
import Text.Parsec hiding (runP,parse)
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Language
    ( emptyDef,
      GenLanguageDef(identLetter, reservedNames),
      LanguageDef )
import Lang

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

natural :: P Int
natural = fromInteger <$> Tok.natural lexer

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

cell :: P Bool
cell = (char '#' >> return True) <|> (char ' ' >> return False)

set :: P Action
set = do a <- do l <- letter
                 case l of
                   'K' -> return Kill
                   'R' -> return Revive
                   c -> fail ("Caracter no reconocido: " ++ [c])
         whiteSpace
         x <- natural
         y <- natural
         return $ a (x, y)

border :: P Int
border = do char '+'
            line <- many1 (char '-')
            char '+'
            whiteSpace
            return (length line)

row :: Int -> P [Int]
row ncols = do char '|'
               line <- count ncols cell
               char '|'
               whiteSpace
               return $ removeDead $ zip [1..] line
  where
    removeDead :: [(Int, Bool)] -> [Int]
    removeDead = map fst . filter snd

-- Corre un parser, chequeando que se pueda consumir toda la entrada
runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (whiteSpace *> p <* eof) () filename s

parseSet :: String -> Either ParseError Action
parseSet s = runP set s ""

-- | Parser de programas
program :: P (Table, Int, Int)
program = do ncols <- border
             rows <- many1 $ row ncols
             border
             whiteSpace
             let positions = removeEmpty $ zip [1..] rows
             return (positions, length rows, ncols)
  where
    removeEmpty :: [(Int, [Int])] -> [Pos]
    removeEmpty = concatMap (uncurry mapZip)
    mapZip :: Int -> [Int] -> [(Int, Int)]
    mapZip n = map (n,)

-----------------------
-- Funciones auxiliares
-----------------------
