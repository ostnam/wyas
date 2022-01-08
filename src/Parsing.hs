module Parsing where

import           Text.ParserCombinators.Parsec
                                         hiding ( spaces )

import           Values

readExpr :: String -> Values.LispOption
readExpr input = case parse parseExpr "lisp" input of
  Left  err -> Err $ Parser err
  Right val -> return val
-- This is the function to call to parse any string to a LispOption

parseExpr :: Parser Values.LispVal
parseExpr = parseNumber <|> parseAtom <|> parseString <|> parseQuoted <|> do
  char '('
  x <- try parseList <|> parseDottedList
  char ')'
  return x
-- This is the top level parser of LispVals

parseAtom :: Parser Values.LispVal
parseAtom = do
  first <- letter <|> symbol
  rest  <- many (letter <|> digit <|> atomSymbol)
  let atom = first : rest
  return $ case atom of
    "True"  -> Bool True
    "False" -> Bool False
    _       -> Atom atom

atomSymbol :: Parser Char
atomSymbol = oneOf "_"
-- Parser for every non-letter or digit valid atom symbol

symbol :: Parser Char
symbol = oneOf "!$%|*+-/!<=?>@^_~#"

spaces :: Parser ()
spaces = skipMany1 space

parseCharLiteral :: Parser Values.LispVal
parseCharLiteral = do
  oneOf "#\\"
  Char <$> letter

parseString :: Parser Values.LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"" <|> letter <|> digit)
  char '"'
  return $ String x

parseNumber :: Parser Values.LispVal
parseNumber =
  try parseNegFloat <|> try parseNegInt <|> try parseFloat <|> parseInt

parseNegFloat :: Parser Values.LispVal
parseNegFloat = do
  char '-'
  Values.Float . (\(Values.Float a) -> -a) <$> parseFloat

parseNegInt :: Parser Values.LispVal
parseNegInt = do
  char '-'
  Values.Int . (\(Values.Int a) -> -a) <$> parseInt

parseFloat :: Parser Values.LispVal
parseFloat = do
  int <- many1 digit
  char '.'
  decimal <- many1 digit
  return $ Float (read (int ++ "." ++ decimal))

parseInt :: Parser Values.LispVal
parseInt = do
  first <- many1 digit
  return $ Int (read first :: Integer)

parseValues :: Parser Values.LispVal
parseValues = Values.List <$> sepBy parseExpr spaces

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

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
