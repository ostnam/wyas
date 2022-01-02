import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Text.Printf

import qualified Parsing
import qualified Values

showArbitraryLispValString :: String -> Bool
showArbitraryLispValString a = Values.showVal (Values.String a) == ("\"" ++ a ++ "\"")
-- Correct string formatting should be the content of the string surrrounded
-- by double quotes.

showArbitraryLispValInt :: Integer -> Bool
showArbitraryLispValInt a = Values.showVal (Values.Int a) == show a

showArbitraryLispValFloat :: Float -> Bool
showArbitraryLispValFloat a = Values.showVal (Values.Float a) == show a

showArbitraryLispValChar :: Char -> Bool
showArbitraryLispValChar a = Values.showVal (Values.Char a) == ['\'', a, '\'']

arbitraryAdd :: [Integer] -> Bool
arbitraryAdd []   = True -- avoid this QuickCheck case which is caught upstream
arbitraryAdd [x] = True -- same
arbitraryAdd ints = Values.polymorphicNumBinop Values.lispAddition (Values.Val . Values.Int <$> ints) == Values.Val (Values.Int (sum ints))

arbitraryAddFloats :: [Float] -> Bool
arbitraryAddFloats []   = True -- avoid this QuickCheck case which is caught upstream
arbitraryAddFloats [x] = True -- same
arbitraryAddFloats floats = Values.polymorphicNumBinop Values.lispAddition (Values.Val . Values.Float <$> floats) == Values.Val (Values.Float (sum floats))

arbitraryAddsIntsAndFloats :: [Integer] -> [Float] -> Bool
arbitraryAddsIntsAndFloats [] []  = True
arbitraryAddsIntsAndFloats a [] = True
arbitraryAddsIntsAndFloats [] a = True
arbitraryAddsIntsAndFloats is fs  = Values.polymorphicNumBinop Values.lispAddition lispValues == Values.Val (Values.Float $ sum $ (fromInteger <$> is) <> fs)
  where lispValues = Values.Val <$> (Values.Float <$> fs) <> (Values.Int <$> is)

arbitraryIntParse :: Integer -> Bool
arbitraryIntParse i = Parsing.readExpr (show i) == Values.Val (Values.Int i)

arbitraryFloatParse :: Float -> Bool
arbitraryFloatParse f = Parsing.readExpr strF == Values.Val (Values.Float f)
  where strF = printf "%f" f

main :: IO ()
main = hspec $ do
  describe "Values" $ do
    describe "show LispVal" $ do
      it "prints atoms" $ do
        Values.showVal (Values.Atom "var_name") `shouldBe` "var_name"
      it "prints strings" $ do
        Values.showVal (Values.String "Hello!") `shouldBe` "\"Hello!\""
      it "prints arbitrary strings" $ do
        property showArbitraryLispValString
      it "prints arbitrary ints" $ do
        property showArbitraryLispValInt
      it "prints arbitrary floats" $ do
        property showArbitraryLispValFloat
      it "prints booleans" $ do
        Values.showVal (Values.Bool True) `shouldBe` "True"
        Values.showVal (Values.Bool False) `shouldBe` "False"
    describe "binaryop" $ do
      it "adds ints" $ do
        property arbitraryAdd
      it "adds floats" $ do
        property arbitraryAddFloats
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

