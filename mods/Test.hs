module Test where


import Data.Char


transform :: String -> String
transform = (++ " That's right!") . (++ "!") . map toUpper


print :: String -> IO ()
print x = putStrLn x