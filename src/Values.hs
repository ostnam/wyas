module Values where

import           Data.Fixed                     ( mod' )
import           Text.ParserCombinators.Parsec
                                         hiding ( spaces )
import           Text.Read                      ( readMaybe )

import           Helper

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Int Integer
             | Float Float
             | String String
             | Char Char
             | Bool Bool
             deriving Eq
-- This is the type for raw, built-in Lisp types.

instance Show LispVal where
  show (String contents) = "\"" ++ contents ++ "\""
  show (Atom   name    ) = name
  show (Int    contents) = show contents
  show (Float  contents) = show contents
  show (Bool   True    ) = "True"
  show (Bool   False   ) = "False"
  show (Char   a       ) = ['\'', a, '\'']
  show (List   contents) = "(" ++ unwordsList contents ++ ")"
  show (DottedList head tail) =
    "(" ++ unwordsList head ++ "." ++ show tail ++ ")"


data LispError = NumArgs Integer [LispOption]
               | TypeMismatch String LispOption
               | Parser ParseError
               | BadSpecialForm String LispOption
               | NotFunction String String
               | UnboundVar String String
               | Default String
               | Numerical String [LispOption]
               deriving Eq
-- This is the type for exceptions.

instance Show LispError where
  show (UnboundVar     message varname) = message ++ ": " ++ varname
  show (BadSpecialForm message form   ) = message ++ ": " ++ show form
  show (NotFunction    message func   ) = message ++ ": " ++ show func
  show (NumArgs expected found) =
    "Expected " ++ show expected ++ " args: found values " ++ unwords
      (show <$> found)
  show (TypeMismatch expected found) =
    "Invalid type: expected "
      ++ expected
      ++ expected
      ++ ", found "
      ++ show found
  show (Parser  parseErr     ) = "Parse error at " ++ show parseErr
  show (Default string       ) = "Error: " ++ show string
  show (Numerical string vals) = "Error:" ++ string ++ unwordsList vals

data LispErrorable a = Err LispError | Val a

instance Show a => Show (LispErrorable a) where
  show (Err a) = show a
  show (Val a) = show a

-- This type is the sum of a lisp exception or another type.

instance Functor LispErrorable where
  fmap f (  Val a) = Val (f a)
  fmap f e@(Err a) = Err a

instance Applicative LispErrorable where
  pure = Val
  (Err a) <*> _       = Err a
  _       <*> (Err a) = Err a
  Val f   <*> Val a   = Val (f a)

instance Monad LispErrorable where
  Err a >>= f = Err a
  Val a >>= f = f a
  return = Val

instance (Eq t) => Eq (LispErrorable t) where
  (==) (Err _) (Val _) = False
  (==) (Val _) (Err _) = False
  (==) (Val a) (Val b) = a == b
  (==) (Err a) (Err b) = a == b

type LispOption = LispErrorable LispVal
-- This is the case of the LispErrorable type applied to LispVals
-- It's the final type for Lisp values.

eval :: LispOption -> LispOption
eval (Err a                         ) = Err a
eval (Val (List [Atom "quote", val])) = Val val
eval (Val (List (Atom func : args) )) = apply func $ eval . Val <$> args
eval (Val val                       ) = Val val

apply :: String -> [LispOption] -> LispOption
apply func args = case lookup func primitives of
  Nothing    -> Err $ NotFunction "Unrecognized primitive function: " func
  Just func' -> func' args


primitives :: [(String, [LispOption] -> LispOption)]
primitives =
  [ ("+"        , lispBinop lispAddition)
  , ("-"        , lispBinop lispSubstraction)
  , ("*"        , lispBinop lispMultiplication)
  , ("/"        , lispBinop lispDivision)
  , ("mod"      , lispBinop lispModulus)
  , ("quotient" , lispBinop lispQuotient)
  , ("remainder", lispBinop lispRemainder)

  , ("=="       , lispStrictBinop lispEq)
  , ("<"        , lispStrictBinop lispInf)
  , (">"        , lispStrictBinop lispSup)
  , ("/="       , lispStrictBinop lispNotEq)
  , (">="       , lispStrictBinop lispSupEq)
  , ("<="       , lispStrictBinop lispInfEq)
  , ("&&"       , lispStrictBinop lispAnd)
  , ("||"       , lispStrictBinop lispOr)
  , ("!"        , lispMonop lispNot)
  ]

lispMonop :: (LispOption -> LispOption) -> [LispOption] -> LispOption
lispMonop op [a] = op a
lispMonop _ _    = Err $ Default "Too many arguments passed to '!'"

lispBinop
  :: (LispOption -> LispOption -> LispOption) -> [LispOption] -> LispOption
lispBinop op singleVal@[Val a] = Err $ NumArgs 2 [Val a]
lispBinop op vals              = foldl1 op vals

lispStrictBinop
  :: (LispOption -> LispOption -> LispOption) -> [LispOption] -> LispOption
lispStrictBinop op [a, b] = op a b
lispStrictBinop _ a       = Err $ NumArgs 2 a

lispAddition :: LispOption -> LispOption -> LispOption
lispAddition a@(Err _)        _                 = a
lispAddition _                a@(Err _        ) = a
lispAddition (Val (Int    a)) (  Val (Int   b)) = Val $ Int (a + b)
lispAddition (Val (Float  a)) (  Val (Float b)) = Val $ Float (a + b)
lispAddition (Val (Int a)) (Val (Float b)) = Val $ Float (fromInteger a + b)
lispAddition (Val (Float a)) (Val (Int b)) = Val $ Float (a + fromInteger b)
lispAddition (Val (String a)) (  Val b        ) = Val $ String (a ++ show b)
lispAddition (Val a         ) (  Val b        ) = Err $ TypeMismatch
  (  "Can't apply the operator + to its arguments passed:"
  ++ show a
  ++ ", and:"
  ++ show b
  )
  (Val $ List [a, b])

lispMultiplication :: LispOption -> LispOption -> LispOption
lispMultiplication a@(Err _)       _                 = a
lispMultiplication _               a@(Err _        ) = a
lispMultiplication (Val (Int   a)) (  Val (Int   b)) = Val $ Int $ a * b
lispMultiplication (Val (Float a)) (  Val (Float b)) = Val $ Float $ a * b
lispMultiplication (Val (Int a)) (Val (Float b)) =
  Val $ Float (fromInteger a * b)
lispMultiplication (Val (Float a)) (Val (Int b)) =
  Val $ Float (a * fromInteger b)
lispMultiplication (Val (String a)) (Val (Int b)) =
  Val $ String $ concat $ replicate (fromInteger b) a
lispMultiplication (Val (Int b)) (Val (String a)) =
  Val $ String $ concat $ replicate (fromInteger b) a
lispMultiplication (Val a) (Val b) = Err $ TypeMismatch
  (  "Can't apply the operator * to its arguments passed:"
  ++ show a
  ++ ", and:"
  ++ show b
  )
  (Val $ List [a, b])

lispDivision :: LispOption -> LispOption -> LispOption
lispDivision a@(Err _) _               = a
lispDivision _         a@(Err _      ) = a
lispDivision (Val a)   (  Val (Int 0)) = Err $ Numerical
  "Tried to divide by 0. Arguments to remainder where:"
  [Val a, Val $ Int 0]
lispDivision (Val a) (Val (Float 0.0)) = Err $ Numerical
  "Tried to divide by 0. Arguments to remainder where:"
  [Val a, Val $ Float 0]
lispDivision (Val (Int a)) (Val (Int b)) =
  Val $ Float (fromInteger a / fromInteger b)
lispDivision (Val (Float a)) (Val (Float b)) = Val $ Float (a / b)
lispDivision (Val (Int   a)) (Val (Float b)) = Val $ Float (fromIntegral a / b)
lispDivision (Val (Float a)) (Val (Int   b)) = Val $ Float (a / fromIntegral b)
lispDivision (Val a) (Val b) =
  Err
    $ TypeMismatch
        "Error: you attempted to calculate the modulus of two types which aren't numerical."
    $ Val
    $ List [a, b]


lispQuotient :: LispOption -> LispOption -> LispOption
lispQuotient a@(Err _)     _               = a
lispQuotient _             a@(Err _      ) = a
lispQuotient (Val (Int a)) (  Val (Int 0)) = Err $ Numerical
  (  "Error: tried to divide by zero while applying quotient of "
  ++ show a
  ++ "divided by: 0"
  )
  [Val $ Int a, Val $ Int 0]
lispQuotient (Val (Int a)) (Val (Int b)) = Val $ Int (div a b)
lispQuotient (Val a      ) (Val b      ) = Err $ TypeMismatch
  (  "Error: tried to calculate the remainder of:"
  ++ show a
  ++ "and "
  ++ show b
  ++ ", but one of them isn't an Int. The remainder requires both arguments to be ints"
  )
  (Val $ List [a, b])

lispRemainder :: LispOption -> LispOption -> LispOption
lispRemainder a@(Err _)     _               = a
lispRemainder _             a@(Err _      ) = a
lispRemainder (Val (Int a)) (  Val (Int 0)) = Err $ Numerical
  (  "Error: tried to divide by zero while applying remainder of "
  ++ show a
  ++ "divided by: 0"
  )
  [Val $ Int a, Val $ Int 0]
lispRemainder (Val (Int a)) (Val (Int b)) = Val $ Int (rem a b)
lispRemainder (Val a      ) (Val b      ) = Err $ TypeMismatch
  (  "Error: tried to calculate the remainder of:"
  ++ show a
  ++ "and "
  ++ show b
  ++ ", but one of them isn't an Int. The remainder requires both arguments to be ints"
  )
  (Val $ List [a, b])

lispModulus :: LispOption -> LispOption -> LispOption
lispModulus a@(Err _) _               = a
lispModulus _         a@(Err _      ) = a
lispModulus (Val a)   (  Val (Int 0)) = Err $ Numerical
  "Tried to divide by 0. Arguments to remainder where:"
  [Val a, Val $ Int 0]
lispModulus (Val a) (Val (Float 0.0)) = Err $ Numerical
  "Tried to divide by 0. Arguments to remainder where:"
  [Val a, Val $ Float 0]
lispModulus (Val (Int   a)) (Val (Int   b)) = Val $ Int (mod a b)
lispModulus (Val (Float a)) (Val (Float b)) = Val $ Float (mod' a b)
lispModulus (Val (Int a)) (Val (Float b)) =
  Val $ Float (mod' (fromIntegral a) b)
lispModulus (Val (Float a)) (Val (Int b)) =
  Val $ Float (mod' a $ fromIntegral b)
lispModulus (Val a) (Val b) =
  Err
    $ TypeMismatch
        "Error: you attempted to calculate the modulus of two types which aren't numerical."
    $ Val
    $ List [a, b]

lispSubstraction :: LispOption -> LispOption -> LispOption
lispSubstraction a@(Err _)       _                 = a
lispSubstraction _               a@(Err _        ) = a
lispSubstraction (Val (Int   a)) (  Val (Int   b)) = Val $ Int (a - b)
lispSubstraction (Val (Float a)) (  Val (Float b)) = Val $ Float (a - b)
lispSubstraction (Val (Int a)) (Val (Float b)) =
  Val $ Float (fromIntegral a - b)
lispSubstraction (Val (Float a)) (Val (Int b)) =
  Val $ Float (a - fromIntegral b)
lispSubstraction (Val a) (Val b) =
  Err
    $ TypeMismatch
        "Error: you attempted to calculate the modulus of two types which aren't numerical."
    $ Val
    $ List [a, b]


lispEq :: LispOption -> LispOption -> LispOption
lispEq a b = Val $ Bool $ a == b

lispNotEq :: LispOption -> LispOption -> LispOption
lispNotEq a b = Val $ Bool $ a /= b

lispSup :: LispOption -> LispOption -> LispOption
lispSup a@(Err _) _ = a
lispSup _ a@(Err _) = a
lispSup (Val (Int a)) (Val (Int b)) = Val $ Bool $ a > b
lispSup (Val (Float a)) (Val (Float b)) = Val $ Bool $ a > b
lispSup (Val (Int a)) (Val (Float b)) = Val $ Bool $ fromInteger a > b
lispSup (Val (Float b)) (Val (Int a)) = Val $ Bool $ fromInteger a > b
lispSup (Val a) (Val b) = Err $ TypeMismatch "Tried comparing two non-numerical values using '>'" (Val $ List [a, b])

lispInf :: LispOption -> LispOption -> LispOption
lispInf a@(Err _) _ = a
lispInf _ a@(Err _) = a
lispInf a b = lispAnd (lispNot $ lispSup a b) (lispNot $ lispEq a b)

lispSupEq :: LispOption -> LispOption -> LispOption
lispSupEq a@(Err _) _ = a
lispSupEq _ a@(Err _) = a
lispSupEq a b = lispOr (lispSup a b) (lispEq a b)

lispInfEq :: LispOption -> LispOption -> LispOption
lispInfEq a@(Err _) _ = a
lispInfEq _ a@(Err _) = a
lispInfEq a b         = lispOr (lispInf a b) (lispEq a b)

lispAnd :: LispOption -> LispOption -> LispOption
lispAnd a@(Err _) _ = a
lispAnd _ a@(Err _) = a
lispAnd (Val (Bool a)) (Val (Bool b)) = Val $ Bool $ a && b
lispAnd (Val a) (Val b) = Err $ TypeMismatch "Tried applying 'and' to one or two non-boolean arguments." (Val $ List [a, b])

lispOr :: LispOption -> LispOption -> LispOption
lispOr a@(Err _) _ = a
lispOr _ a@(Err _) = a
lispOr (Val (Bool a)) (Val (Bool b)) = Val $ Bool $ a || b
lispOr (Val a) (Val b) = Err $ TypeMismatch "Tried applying 'and' to one or two non-boolean arguments." (Val $ List [a, b])

lispNot :: LispOption -> LispOption
lispNot a@(Err _) = a
lispNot (Val (Bool a))  = Val $ Bool $ not a
lispNot a  = Err $ TypeMismatch "Tried applying '!' to a non-boolean arg:" a

lispOptFloatEq :: LispOption -> LispOption -> Bool
lispOptFloatEq (Val (Float a)) (Val (Float b)) = floatEq a b
lispOptFloatEq _               _               = False
