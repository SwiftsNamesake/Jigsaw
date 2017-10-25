-- |
-- Module      : Main
-- Description : 
-- Copyright   : (c) Jonatan Sundqvist, year
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
import Data.Functor ((<$>))
import Data.Dynamic
import Data.Map as M

import Plugins

-- Definitions -----------------------------------------------------------------

-- |
explain :: a -> Maybe b -> Either a b
explain a = maybe (Left a) Right

-- |
loadRun :: EitherT PluginError IO (Plugin (String -> String))
loadRun = do
  plugin <- EitherT $ Plugins.load "./mods/Test.hs" "Test" ["transform"]
  EitherT . return $ Plugin <$> explain LookupFailure (M.lookup "transform" plugin >>= fromDynamic)

-- |
main :: IO ()
main = (either (show) (($ "Hey") . run) <$> runEitherT loadRun) >>= putStrLn