-- |
-- Module      : Main
-- Description : 
-- Copyright   : (c) Jonatan Sundqvist, 2016
-- License     : MIT
-- Maintainer  : Jonatan Sundqvist
-- Stability   : $
-- Portability : $
--

-- TODO | - 
--        - 

-- GHC Pragmas -----------------------------------------------------------------

-- API -------------------------------------------------------------------------

module Main where

-- We'll need these ------------------------------------------------------------

import Control.Applicative
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import Data.Functor ((<$>))
import Data.Dynamic
import Data.Monoid

import System.IO

import Jigsaw.Load as Jigsaw

-- Definitions -----------------------------------------------------------------

-- |
loadRun :: EitherT PluginError IO [String -> String]
loadRun = Jigsaw.load "./mods/Test.hs" "Test" ["transforms"] >>= \m -> EitherT . return $ lookupSymbol m "transforms"

-- |
prompt :: String -> IO String
prompt s = putStr s *> hFlush stdout *> getLine

-- |
main :: IO ()
main = do
  ln <- prompt "Say something: "
  either show (run ln) <$> runEitherT loadRun >>= putStrLn
  where
    run ln = unlines . zipWith (\n f -> "(" <> show n <> ") " <> f ln) [1..]