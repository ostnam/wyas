import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

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
arbitraryAdd ints = Values.numericBinop (+) (map Values.Int ints) == (Values.Val $ Values.Int (foldl1 (+) ints))

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
  describe "Parsing" $ do
    it "parses booleans" $ do
        Parsing.readExpr "True" `shouldBe` Values.Val (Values.Bool True)
        Parsing.readExpr "False" `shouldBe` Values.Val (Values.Bool False)


