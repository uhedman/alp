{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module Parse (runP, P, parsePut, parseResize, program) where

import Prelude hiding ( const )
import Text.Parsec hiding (State, runP,parse)
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Language
    ( emptyDef,
      GenLanguageDef(identLetter, reservedNames),
      LanguageDef )
import Lang
import Data.Maybe (fromJust, isJust)
import Data.Bifunctor (second)

type P = Parsec String ()

-----------------------
-- Lexer
-----------------------
-- | Analizador de Tokens
lexer :: Tok.TokenParser u
lexer = Tok.makeTokenParser langDef

langDef :: LanguageDef u
langDef = emptyDef {
         reservedNames = ["+", "|", "-"]
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

cell :: [Char] -> P (Maybe Char)
cell lang = (do l <- oneOf lang
                return (Just l)) <|> (space >> return Nothing)

put :: P (Pos, State)
put = do l <- satisfy (/= ' ')
         whiteSpace
         x <- natural
         y <- natural
         return ((x,y), l)

resize :: P (Int, Int)
resize = do x <- natural
            y <- natural
            return (x,y)

border :: P Int
border = do char '+'
            line <- many1 (char '-')
            char '+'
            whiteSpace
            return (length line)

row :: Int -> [Char] -> P [(Int, State)]
row ncols lang = do char '|'
                    line <- count ncols (cell lang)
                    char '|'
                    whiteSpace
                    return $ removeEmpty $ zip [1..] line
  where
    removeEmpty :: [(Int, Maybe State)] -> [(Int, State)]
    removeEmpty = map (second fromJust) . filter (\(_, v) -> isJust v)

-- Corre un parser, chequeando que se pueda consumir toda la entrada
runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (whiteSpace *> p <* eof) () filename s

parsePut :: String -> Either ParseError (Pos, State)
parsePut s = runP put s ""

parseResize :: String -> Either ParseError (Int, Int)
parseResize s = runP resize s ""

-- | Parser de programas
program :: [Char] -> P (Table, Int, Int)
program lang = do ncols <- border
                  rows <- many1 $ row ncols lang
                  border
                  whiteSpace
                  let positions = concatMap (\(r, cols) -> [((r, c), s) | (c, s) <- cols]) $ zip [1..] rows
                  return (positions, length rows, ncols)

-----------------------
-- Funciones auxiliares
-----------------------
