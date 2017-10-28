module Test where

import Data.List
import Data.Char

transforms :: [String -> String]
transforms = [(++ " is actually really offensive") . (++ "'") . ("'" ++)
             ,(++ " is a blatant lie!")
             ,(transforms !! 1) . (transforms !! 4)
             ,(++ "?")
             ,fmap toUpper
             ,intersperse ' ' . (transforms !! 4)
             ,(++ " is a vile hate crime")
             ,unlines . fmap (intersperse ' ') . transpose . words]