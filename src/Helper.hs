module Helper where

import           Control.Exception              ( TypeError(TypeError) )

floatEq :: Float -> Float -> Bool
floatEq a b =
  ((b * 1.01 > a) && (a * 1.01 > b)) || ((a + 1 > b) && (b + 1 > a))


unwordsList :: (Show a) => [a] -> String
unwordsList = unwords . (show <$>)
