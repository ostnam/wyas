module Parsing where

import           Control.Monad.Error
import           Text.ParserCombinators.Parsec hiding (spaces)

import           Values

symbol :: Parser Char
symbol = oneOf "!$%|*+-/!<=?>@^_~#"

readExpr :: String -> Values.LispOption
readExpr input = case parse parseExpr "lisp" input of
  Left err  -> Err $ Parser err
  Right val -> return val

spaces :: Parser ()
spaces = skipMany1 space

parseCharLiteral :: Parser Values.LispVal
parseCharLiteral = do
  oneOf "#\\"
  Char <$> letter

parseString :: Parser Values.LispVal
parseString = do
  char '"'
  x <- many (noneOf "\""
         <|> oneOf "\\\""
         <|> oneOf "\t"
         <|> oneOf "\n"
         <|> oneOf "\\")
  char '"'
  return $ String x

parseAtom :: Parser Values.LispVal
parseAtom = do first <- letter <|> symbol
               rest  <- many (letter <|> digit <|> symbol)
               let atom = first : rest
               return $ case atom of
                 "#t" -> Bool True
                 "#f" -> Bool False
                 _    -> Atom atom

parseNumber :: Parser Values.LispVal
parseNumber =  try parseFloat
           <|> parseInt

parseFloat :: Parser Values.LispVal
parseFloat = do
  int <- many1 (oneOf "0123456789")
  char '.'
  decimal <- many1 (oneOf "0123456789")
  return $ Float (read (int ++ "." ++ decimal))

parseInt :: Parser Values.LispVal
parseInt = do
  first <- many1 digit
  return $ Int (read first :: Integer)

parseExpr :: Parser Values.LispVal
parseExpr =  parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

parseValues :: Parser Values.LispVal
parseValues = Values.List <$> sepBy parseExpr spaces

parseList :: Parser LispVal
parseList = fmap List $ sepBy parseExpr spaces

parseDottedList :: Parser Values.LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser Values.LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ Values.List [Atom "quote", x]
