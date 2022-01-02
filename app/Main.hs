module Main where

import qualified System.Environment
import Parsing
import Values

main :: IO ()
main = do
  args <- System.Environment.getArgs
  let evaled = fmap show $ eval $ readExpr (head args)
  print evaled
