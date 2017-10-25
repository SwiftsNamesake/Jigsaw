module Test where


import Data.Char


transform :: String -> String
transform = (++ " is actually really offensive") . (++ "'") . ("'" ++)


print :: String -> IO ()
print x = putStrLn x