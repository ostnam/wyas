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
                                      then case Values.polymorphicNumBinop Values.lispModulus lispValues of
                                                                (Values.Err (Values.Numerical _ _)) -> True
                                                                _                                   -> False
                                      else Values.polymorphicNumBinop Values.lispModulus lispValues ==
                                              Values.Val (Values.Float $ foldl1 mod' joined)
  where lispValues = Values.Val <$> (Values.Int <$> (i:is)) <> (Values.Float <$> (f:fs))
        joined     = (fromIntegral <$> (i:is)) <> (f:fs)

arbitraryRems :: [Integer] -> Bool
arbitraryRems [] = True
arbitraryRems [a] = case Values.intBinop Values.lispRemainder [Values.Val $ Values.Int a] of
                      (Values.Err _) -> True
                      _              -> False
arbitraryRems (x:xs) = if 0 `elem` xs
                         then case Values.intBinop Values.lispRemainder lispValues of
                                      (Values.Err _) -> True
                                      _              -> False
                         else Values.intBinop Values.lispRemainder lispValues ==
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

