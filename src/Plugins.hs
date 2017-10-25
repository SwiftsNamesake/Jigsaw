-- |
-- Module      : Plugins
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

module Plugins where

-- We'll need these ------------------------------------------------------------

import Data.Maybe (maybe)
import Data.Monoid ((<>))
import Data.Functor ((<$>))
import Data.Dynamic
import Data.Typeable
import qualified Data.Map.Strict as M

import GHC
import GHC.Paths
import DynFlags
import Unsafe.Coerce

-- Definitions -----------------------------------------------------------------

-- |
-- TODO | - Safety
--        - Probably lots of other things I haven't thought of yet
load :: String -> String -> [String] -> IO (Either PluginError (M.Map String Dynamic))
load fn modname symbols = defaultErrorHandler putStrLn (FlushOut $ putStrLn "Something went awry. Flushing out.") $ do
  runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    setSessionDynFlags dflags
    target <- guessTarget fn Nothing
    addTarget target
    r <- GHC.load LoadAllTargets
    case r of
      Failed    -> return $ Left LoadFailure
      Succeeded -> do
        setContext [IIDecl $ simpleImportDecl (mkModuleName modname)]
        (Right . M.fromList . zip symbols) <$> mapM (\s -> dynCompileExpr $ modname <> "." <> s) symbols -- A list of 'dynamic imports'

-- Types -----------------------------------------------------------------------

-- |
data Plugin a = Plugin { run :: a } deriving Show

-- |
data PluginError = LoadFailure | LookupFailure deriving Show

-- Functions -------------------------------------------------------------------

---- |
--rawLoad = do
--	target <- GHC.guessTarget "mods/Test.hs" Nothing
--	GHC.addTarget target
--	r <- GHC.load GHC.LoadAllTargets
--	case r of
--		GHC.Failed    -> error "Compilation failed"
--		GHC.Succeeded -> do
--			m <- GHC.findModule (GHC.mkModuleName "Test") Nothing
--			GHC.setContext [] [m]
--			value <- GHC.compileExpr "Test.print"
--			return value--

---- |
--load :: String -> IO (Maybe (Plugin a))
--load fn = return $ Nothing
