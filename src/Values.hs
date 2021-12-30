module Values (module Values) where

import           Text.ParserCombinators.Parsec hiding (spaces)
import Text.Read (readMaybe)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Int Integer
             | Float Float
             | String String
             | Char Char
             | Bool Bool

instance Show LispVal where show = showVal

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String


data LispErrorable a = Err LispError | Val a
  deriving Show

instance Functor LispErrorable where
  fmap f (Val a) = Val (f a)
  fmap f (Err a) = Err a

instance Applicative LispErrorable where
  pure a = Val a
  (Err a) <*> _ = Err a
  _ <*> (Err a) = Err a
  Val f <*> Val a = Val (f a)

instance Monad LispErrorable where
  Err a >>= f = Err a
  Val a >>= f = f a
  return      = Val


reduceBinop :: (a -> a -> a)
            -> LispErrorable a
            -> LispErrorable a
            -> LispErrorable a
reduceBinop _ x@(Err _) _      = x
reduceBinop _ _ x@(Err _)      = x
reduceBinop op (Val x) (Val y) = Val (op x y)

type LispOption = LispErrorable LispVal

instance Show LispError where
  show (UnboundVar message varname) = message ++ ": " ++ varname
  show (BadSpecialForm message form) = message ++ ": " ++ show form
  show (NotFunction message func) = message ++ ": " ++ show func
  show (NumArgs expected found) = "Expected " ++ show expected
                               ++ " args: found values " ++ unwordsList found
  show (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                    ++ expected ++ ", found " ++ show found
  show (Parser parseErr) = "Parse error at " ++ show parseErr
  show (Default string) = "Error: " ++ show string

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Int contents) = show contents
showVal (Float contents) = show contents
showVal (Bool True) = "True"
showVal (Bool False) = "False"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ "." ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

eval :: LispVal -> LispOption
eval val@(String _) = return val
eval val@(Int _) = return val
eval val@(Float _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval a = Err $ BadSpecialForm "Unrecognized special form" a

apply :: String -> [LispVal] -> LispOption
apply func args =
  case lookup func primitives of
    Nothing    -> Err $ NotFunction "Unrecognized primitive function: "  func
    Just func' -> func' args

primitives :: [(String, [LispVal] -> LispOption)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer)
             -> [LispVal]
             -> LispOption
numericBinop op singleVal@[_] = Err $ NumArgs 2 singleVal
numericBinop op params =
  case foldBinopLispOption op (unpackNums <$> params) of
    Err a -> Err a
    Val a -> Val $ Int a

foldBinopLispOption :: (a -> a -> a)
                    -> [LispErrorable a]
                    -> LispErrorable a
foldBinopLispOption op = foldl1 (reduceBinop op)


unpackNums :: LispVal -> LispErrorable Integer
unpackNums num = case readMaybe (show num) :: Maybe Integer of
                   Nothing -> Err $ Default $ "Error: couldn't parse " ++ show num ++ " to a number."
                   Just i  -> Val i