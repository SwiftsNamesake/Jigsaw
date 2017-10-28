module Test where


import Data.Char

transforms :: [String -> String]
transforms = [(++ " is actually really offensive") . (++ "'") . ("'" ++)
             ,(++ " is a blatant lie!")
             ,(transforms !! 1) . fmap toUpper
             ,(++ "?")]


print :: String -> IO ()
print x = putStrLn x