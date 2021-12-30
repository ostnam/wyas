import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import qualified Parsing
import qualified Values

arbitraryString :: String -> Bool 
arbitraryString a = Values.showVal (Values.String a) == ("\"" ++ a ++ "\"")

main :: IO ()
main = hspec $
  describe "Values.showVal" $ do
  it "prints atoms" $ do
    Values.showVal (Values.Atom "var_name") `shouldBe` "var_name"
  it "prints strings" $ do
    Values.showVal (Values.String "Hello!") `shouldBe` "\"Hello!\""
  it "prints arbitrary strings" $ do
    property  arbitraryString
  it "prints booleans" $ do
    Values.showVal (Values.Bool True) `shouldBe` "True"
    Values.showVal (Values.Bool False) `shouldBe` "False"

