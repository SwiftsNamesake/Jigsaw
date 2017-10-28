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
import Data.Map as M

import Plugins

-- Definitions -----------------------------------------------------------------

-- |
loadRun :: EitherT PluginError IO [String -> String]
loadRun = Plugins.load "./mods/Test.hs" "Test" ["transforms"] >>= \m -> EitherT . return $ lookupSymbol m "transforms"

-- |
main :: IO ()
main = do
  ln <- getLine
  (either (show) (unlines . fmap ($ ln)) <$> runEitherT loadRun) >>= putStrLn