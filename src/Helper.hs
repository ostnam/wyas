module Helper where

unwordsList :: Show a => [a] -> String
unwordsList = unwords . map show

