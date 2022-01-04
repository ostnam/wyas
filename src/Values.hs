module Values where

import           Text.ParserCombinators.Parsec hiding (spaces)
import           Text.Read                     (readMaybe)
import           Data.Fixed (mod')

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Int Integer
             | Float Float
             | String String
             | Char Char
             | Bool Bool
             deriving Eq
-- This is the datatype representing every basic datatype.

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Int contents) = show contents
showVal (Float contents) = show contents
showVal (Bool True) = "True"
showVal (Bool False) = "False"
showVal (Char a) = ['\'', a, '\'']
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ "." ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

data LispError = NumArgs Integer [LispOption]
               | TypeMismatch String LispOption
               | Parser ParseError
               | BadSpecialForm String LispOption
               | NotFunction String String
               | UnboundVar String String
               | Default String
               | Numerical String [LispOption]
               deriving Eq

instance Show LispError where
  show (UnboundVar message varname) = message ++ ": " ++ varname
  show (BadSpecialForm message form) = message ++ ": " ++ show form
  show (NotFunction message func) = message ++ ": " ++ show func
  show (NumArgs expected found) = "Expected " ++ show expected
                               ++ " args: found values " ++ unwords (show <$> found)

  show (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                    ++ expected ++ ", found " ++ show found
  show (Parser parseErr) = "Parse error at " ++ show parseErr
  show (Default string) = "Error: " ++ show string

data LispErrorable a = Err LispError | Val a
  deriving Show

instance (Eq t) => Eq (LispErrorable t) where
  (==) (Err _) (Val _) = False
  (==) (Val _) (Err _) = False
  (==) (Val a) (Val b) = a == b
  (==) (Err a) (Err b) = a == b

instance Functor LispErrorable where
  fmap f (Val a) = Val (f a)
  fmap f e@(Err a) = Err a

instance Applicative LispErrorable where
  pure a = Val a
  (Err a) <*> _ = Err a
  _ <*> (Err a) = Err a
  Val f <*> Val a = Val (f a)

instance Monad LispErrorable where
  Err a >>= f = Err a
  Val a >>= f = f a
  return      = Val

type LispOption = LispErrorable LispVal
-- This is the case of the LispErrorable type applied to LispVals


eval :: LispOption -> LispOption
eval (Err a) = Err a
eval (Val (List [Atom "quote", val])) = Val val
eval (Val (List (Atom func : args))) = apply func $ eval . Val <$> args
eval (Val val) = Val val

apply :: String -> [LispOption] -> LispOption
apply func args =
  case lookup func primitives of
    Nothing    -> Err $ NotFunction "Unrecognized primitive function: "  func
    Just func' -> func' args


primitives :: [(String, [LispOption] -> LispOption)]
primitives = [("+", polymorphicNumBinop lispAddition),
              ("-", polymorphicNumBinop lispSubstraction),
              ("*", polymorphicNumBinop lispMultiplication),
              ("/", polymorphicNumBinop lispDivision),
              ("mod", polymorphicNumBinop lispModulus),
              ("quotient", polymorphicNumBinop lispQuotient),
              ("remainder", intBinop lispRemainder)]

polymorphicNumBinop :: (LispErrorable LispPolymorphicNum ->
                        LispErrorable LispPolymorphicNum ->
                        LispErrorable LispPolymorphicNum)
                    -> [LispOption]
                    -> LispOption
polymorphicNumBinop op singleVal@[Val a] = Err $ NumArgs 2 [Val a]
polymorphicNumBinop op vals =
  case foldl1 op (toPolymorphicNum <$> vals) of
    Err a -> Err a
    Val (Left a)  -> Val $ Int a
    Val (Right a) -> Val $ Float a

intBinop :: (LispOption -> LispOption -> LispOption)
         -> [LispOption]
         -> LispOption
intBinop op singleval@[Val a] = Err $ NumArgs 2 [Val a]
intBinop op vals = foldl1 op vals

toPolymorphicNum :: LispOption -> LispErrorable LispPolymorphicNum
toPolymorphicNum (Val (Int a)) = Val $ Left a
toPolymorphicNum (Val (Float a)) = Val $ Right a
toPolymorphicNum a = Err $ TypeMismatch ("Couldn't apply a numerical function to" ++ show a ++ ", as it isn't a numerical value.") a

lispAddition :: LispErrorable LispPolymorphicNum
        -> LispErrorable LispPolymorphicNum
        -> LispErrorable LispPolymorphicNum
lispAddition a@(Err _) _ = a
lispAddition _ a@(Err _) = a
lispAddition (Val (Left a)) (Val (Left b)) = Val $ Left (a + b)
lispAddition (Val (Right a)) (Val (Right b)) = Val $ Right (a + b)
lispAddition (Val (Left a)) (Val (Right b)) = Val $ Right (fromInteger a + b)
lispAddition (Val (Right a)) (Val (Left b)) = Val $ Right (a + fromInteger b)

lispMultiplication :: LispErrorable LispPolymorphicNum -> LispErrorable LispPolymorphicNum -> LispErrorable LispPolymorphicNum
lispMultiplication = error "not implemented"

lispRemainder :: LispOption 
              -> LispOption
              -> LispOption
lispRemainder a@(Err _) _ = a
lispRemainder _ a@(Err _) = a
lispRemainder (Val (Int a)) (Val (Int 0)) = Err $ Numerical ("Error: tried to divide by zero while applying remainder of " ++ show a ++ "divided by: 0") [Val $ Int a, Val $ Int 0]
lispRemainder (Val (Int a)) (Val (Int b)) = Val $ Int (rem a b)
lispRemainder (Val a) (Val b)             = Err $ TypeMismatch ("Error: tried to calculate the remainder of:" ++ show a ++ "and " ++ show b ++ ", but one of them isn't an Int. The remainder requires both arguments to be ints") (Val $ List [a, b])

lispQuotient :: LispErrorable LispPolymorphicNum -> LispErrorable LispPolymorphicNum -> LispErrorable LispPolymorphicNum
lispQuotient = error "not implemented"

lispModulus :: LispErrorable LispPolymorphicNum -> LispErrorable LispPolymorphicNum -> LispErrorable LispPolymorphicNum
lispModulus a@(Err _) _ = a
lispModulus _ a@(Err _) = a
lispModulus (Val a) (Val (Left 0)) = Err $ Numerical "Tried to divide by 0. Arguments to remainder where:" [polymorphicNumToLispOption a, Val $ Int 0]
lispModulus (Val a) (Val (Right 0.0)) = Err $ Numerical "Tried to divide by 0. Arguments to remainder where:" [polymorphicNumToLispOption a, Val $ Float 0]
lispModulus (Val (Left a)) (Val (Left b)) = Val $ Left (mod a b)
lispModulus (Val (Right a)) (Val (Right b)) = Val $ Right (mod' a b)
lispModulus (Val (Left a)) (Val (Right b)) = Val $ Right (mod' (fromIntegral a) b)
lispModulus (Val (Right a)) (Val (Left b)) = Val $ Right (mod' a $ fromIntegral b)

lispDivision :: LispErrorable LispPolymorphicNum -> LispErrorable LispPolymorphicNum -> LispErrorable LispPolymorphicNum
lispDivision = error "not implemented"

lispSubstraction :: LispErrorable LispPolymorphicNum -> LispErrorable LispPolymorphicNum -> LispErrorable LispPolymorphicNum
lispSubstraction = error "not implemented"

type LispPolymorphicNum = Either Integer Float

polymorphicNumToLispOption :: LispPolymorphicNum -> LispOption
polymorphicNumToLispOption (Left a) = Val $ Int a 
polymorphicNumToLispOption (Right a) = Val $ Float a 
