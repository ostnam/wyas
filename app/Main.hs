module Main where

import qualified System.Environment
import Parsing
import Values
import Repl

main :: IO ()
main = do
  args <- System.Environment.getArgs
  case length args of
    0 -> runRepl
    1 -> evalAndPrint $ head args
    _ -> putStrLn "Program takes 0 or 1 argument"
  {-
  let evaled = fmap show $ eval $ readExpr (head args)
  print evaled
  -}
