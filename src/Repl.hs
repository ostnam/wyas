module Repl where

import System.IO hiding (try)

import Parsing
import Values

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return $ show $ eval $ readExpr expr

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool)
                  -> m a
                  -> (a -> m ())
                  -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
     then return ()
     else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp> ") evalAndPrint
