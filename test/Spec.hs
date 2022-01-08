import           Control.Exception (evaluate)
import           Test.Hspec
import           Test.QuickCheck

import           Data.Fixed        (mod')
import           Text.Printf

import qualified Parsing
import qualified Values

showArbitraryLispValString :: String -> Bool
showArbitraryLispValString a = show (Values.String a) == ("\"" ++ a ++ "\"")
-- Correct string formatting should be the content of the string surrrounded
-- by double quotes.

showArbitraryLispValInt :: Integer -> Bool
showArbitraryLispValInt a = show (Values.Int a) == show a

showArbitraryLispValFloat :: Float -> Bool
showArbitraryLispValFloat a = show (Values.Float a) == show a

showArbitraryLispValChar :: Char -> Bool
showArbitraryLispValChar a = show (Values.Char a) == ['\'', a, '\'']

arbitraryAdd :: [Integer] -> Bool
arbitraryAdd []   = True -- avoid this QuickCheck case which is caught upstream
arbitraryAdd [x] = True -- same
arbitraryAdd ints = Values.lispBinop Values.lispAddition (Values.Val . Values.Int <$> ints) == Values.Val (Values.Int (sum ints))

arbitraryAddFloats :: [Float] -> Bool
arbitraryAddFloats []   = True -- avoid this QuickCheck case which is caught upstream
arbitraryAddFloats [x] = True -- same
arbitraryAddFloats floats = Values.lispBinop Values.lispAddition (Values.Val . Values.Float <$> floats) == Values.Val (Values.Float (sum floats))

arbitraryModsIntsAndFloats :: [Integer] -> [Float] -> Bool
arbitraryModsIntsAndFloats [] []  = True
arbitraryModsIntsAndFloats a [] = True
arbitraryModsIntsAndFloats [] a = True
arbitraryModsIntsAndFloats (i:is) (f:fs)  = if 0.0 `elem` tail joined
                                      then case Values.lispBinop Values.lispModulus lispValues of
                                                                (Values.Err (Values.Numerical _ _)) -> True
                                                                _                                   -> False
                                      else Values.lispBinop Values.lispModulus lispValues ==
                                              Values.Val (Values.Float $ foldl1 mod' joined)
  where lispValues = Values.Val <$> (Values.Int <$> (i:is)) <> (Values.Float <$> (f:fs))
        joined     = (fromIntegral <$> (i:is)) <> (f:fs)

arbitraryRems :: [Integer] -> Bool
arbitraryRems [] = True
arbitraryRems [a] = case Values.lispBinop Values.lispRemainder [Values.Val $ Values.Int a] of
                      (Values.Err _) -> True
                      _              -> False
arbitraryRems (x:xs) = if 0 `elem` xs
                         then case Values.lispBinop Values.lispRemainder lispValues of
                                      (Values.Err _) -> True
                                      _              -> False
                         else Values.lispBinop Values.lispRemainder lispValues ==
                                Values.Val (Values.Int $ foldl1 rem (x:xs))
  where lispValues = Values.Val <$> (Values.Int <$> (x:xs))

arbitraryAddsIntsAndFloats :: [Integer] -> [Float] -> Bool
arbitraryAddsIntsAndFloats [] [] = True
arbitraryAddsIntsAndFloats a []  = True
arbitraryAddsIntsAndFloats [] a  = True
arbitraryAddsIntsAndFloats is fs = Values.lispBinop Values.lispAddition lispValues ==
                                      Values.Val (Values.Float $ sum $ (fromInteger <$> is) <> fs)
  where lispValues = Values.Val <$> (Values.Int <$> is) <> (Values.Float <$> fs)

arbitraryIntParse :: Integer -> Bool
arbitraryIntParse i = Parsing.readExpr (show i) == Values.Val (Values.Int i)

arbitraryFloatParse :: Float -> Bool
arbitraryFloatParse f = Parsing.readExpr strF == Values.Val (Values.Float f)
  where strF = printf "%f" f

{-
arbitraryMultIntAndFloat :: [Integer]
                        -> [Float]
                        -> Bool
arbitraryMultIntAndFloat [] [] = True
arbitraryMultIntAndFloat a []  = True
arbitraryMultIntAndFloat [] a  = True
arbitraryMultIntAndFloat is fs = Values.lispBinop Values.lispMultiplication lispValues `Values.lispOptFloatEq`
                                      Values.Val (Values.Float $ product $ (fromInteger <$> is) <> fs)
  where lispValues = Values.Val <$> (Values.Int <$> is) <> (Values.Float <$> fs)
-}
repeatStr :: String
          -> Integer
          -> Bool
repeatStr str int = Values.lispBinop Values.lispMultiplication vals ==
                      Values.Val (Values.String $ concat $ replicate (fromInteger int) str)
  where vals = Values.Val <$> [Values.String str, Values.Int int]

arbitrarySubstractIntAndFloat :: [Integer]
                              -> [Float]
                              -> Bool
arbitrarySubstractIntAndFloat [] [] = True
arbitrarySubstractIntAndFloat a []  = True
arbitrarySubstractIntAndFloat [] a  = True
arbitrarySubstractIntAndFloat is fs = Values.lispBinop Values.lispSubstraction lispValues `Values.lispOptFloatEq`
                                      Values.Val (Values.Float $ foldl1 (-) $ (fromInteger <$> is) <> fs)
  where lispValues = Values.Val <$> (Values.Int <$> is) <> (Values.Float <$> fs)

arbitraryDivideIntAndFloat :: [Integer]
                              -> [Float]
                              -> Bool
arbitraryDivideIntAndFloat [] [] = True
arbitraryDivideIntAndFloat [] [a] = case Values.lispBinop Values.lispDivision [Values.Val $ Values.Float a] of
                      (Values.Err _) -> True
                      _              -> False
arbitraryDivideIntAndFloat [a] [] = case Values.lispBinop Values.lispDivision [Values.Val $ Values.Int a] of
                      (Values.Err _) -> True
                      _              -> False
arbitraryDivideIntAndFloat is fs = if 0.0 `elem` tail joined
                         then case Values.lispBinop Values.lispDivision lispValues of
                                      (Values.Err _) -> True
                                      _              -> False
                         else Values.lispBinop Values.lispDivision lispValues `Values.lispOptFloatEq`
                                Values.Val (Values.Float $ foldl1 (/) joined)
  where lispValues = Values.Val <$> (Values.Int <$> is) <> (Values.Float <$> fs)
        joined = (fromInteger <$> is) <> fs

arbitraryIntDivision :: [Integer]
                     -> Bool
arbitraryIntDivision [] = True
arbitraryIntDivision [a] = case Values.lispBinop Values.lispQuotient [Values.Val $ Values.Int a] of
                      (Values.Err _) -> True
                      _              -> False
arbitraryIntDivision (x:xs) = if 0 `elem` xs
                         then case Values.lispBinop Values.lispQuotient lispValues of
                                      (Values.Err _) -> True
                                      _              -> False
                         else Values.lispBinop Values.lispQuotient lispValues ==
                                Values.Val (Values.Int $ foldl1 div (x:xs))
  where lispValues = Values.Val . Values.Int <$> (x:xs)


main :: IO ()
main = hspec $ do
  describe "Values" $ do
    describe "show LispVal" $ do
      it "prints atoms" $ do
        show (Values.Atom "var_name") `shouldBe` "var_name"
      it "prints strings" $ do
        show (Values.String "Hello!") `shouldBe` "\"Hello!\""
      it "prints arbitrary strings" $ do
        property showArbitraryLispValString
      it "prints arbitrary ints" $ do
        property showArbitraryLispValInt
      it "prints arbitrary floats" $ do
        property showArbitraryLispValFloat
      it "prints booleans" $ do
        show (Values.Bool True) `shouldBe` "True"
        show (Values.Bool False) `shouldBe` "False"

    describe "binaryop" $ do
      it "adds ints" $ do
        property arbitraryAdd
      it "adds floats" $ do
        property arbitraryAddFloats
      it "adds ints and floats together" $ do
        property arbitraryAddsIntsAndFloats
      it "mods ints and floats together" $ do
        property arbitraryModsIntsAndFloats
      it "rems ints and floats together" $ do
        property arbitraryRems
      it "multiplies ints and floats together" $ do
        pendingWith "need to deal with float precision errors"  -- property arbitraryMultIntAndFloat
      it "multiplies strings and ints" $ do
        property repeatStr
      it "substracts ints and floats" $ do
        property arbitrarySubstractIntAndFloat
      it "divides ints and floats" $ do
        property arbitraryDivideIntAndFloat
      it "does integer division with quotient" $ do
        property arbitraryIntDivision




  describe "Parsing" $ do
    it "parses booleans" $ do
      Parsing.readExpr "True" `shouldBe` Values.Val (Values.Bool True)
      Parsing.readExpr "False" `shouldBe` Values.Val (Values.Bool False)
    it "parses strings" $ do
      Parsing.readExpr "\"hello\"" `shouldBe` Values.Val (Values.String "hello")
    it "parses ints" $ do
      property arbitraryIntParse
    it "parses floats" $ do
      property arbitraryFloatParse
    it "parses atoms" $ do
      Parsing.readExpr "atom" `shouldBe` Values.Val (Values.Atom "atom")
    it "parses lists" $ do
      Parsing.readExpr "(+ 3 2 \"str\")"  `shouldBe` Values.Val (Values.List [Values.Atom "+", Values.Int 3, Values.Int 2, Values.String "str"])

