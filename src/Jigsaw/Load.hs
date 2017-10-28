-- |
-- Module      : Jigsaw.Load
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

{-# LANGUAGE TupleSections #-}

-- API -------------------------------------------------------------------------

module Jigsaw.Load where

-- We'll need these ------------------------------------------------------------

import Data.Maybe (maybe)
import Data.Monoid ((<>))
import Data.Functor ((<$>))
import Data.Dynamic
import Data.Typeable
import Data.Functor ((<$>))
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)

import Control.Monad.Trans.Either
import Control.Monad.IO.Class

import GHC
import GHC.Paths
import DynFlags
import Unsafe.Coerce

-- Definitions -----------------------------------------------------------------

-- |
-- TODO | - Safety
--        - Dependencies
--        - Introspection
--        - Evaluating expressions
--        - Performance
--        - Simplify the interface (eg. remove 'modname' and possibly 'symbols')
--        - Refactor
--        - Logging, recovery
--        - Sandboxing (SafeHaskell, TH, unsafePerformIO, memory and timeout, etc.)
--        - Probably lots of other things I haven't thought of yet
load :: String -> String -> [String] -> EitherT PluginError IO DynamicModule
load fn modname symbols = EitherT . onError . runGhc (Just libdir) $ do
  dflags <- getSessionDynFlags
  setSessionDynFlags $ addReloadFlags dflags
  target <- guessTarget fn Nothing
  addTarget target
  outcome <- GHC.load LoadAllTargets
  case outcome of
    Failed    -> return $ Left LoadFailure
    Succeeded -> do
      setContext [IIDecl $ simpleImportDecl (mkModuleName modname)]
      Right . Map.fromList <$> mapM compileSymbol symbols
  where
    onError = defaultErrorHandler putStrLn (FlushOut $ putStrLn "Something went awry. Flushing out.")
    makeName s =  modname <> "." <> s
    compileSymbol s = (s,) <$> (dynCompileExpr $ makeName s)
    addReloadFlags fs = fs { ghcLink      = LinkInMemory,
                             hscTarget    = HscInterpreted,
                             packageFlags = [ExposePackage (PackageArg "ghc") (ModRenaming True [])] }

-- |
explain :: a -> Maybe b -> Either a b
explain a = maybe (Left a) Right

-- |
lookupSymbol :: Typeable a => DynamicModule -> String -> Either PluginError a
lookupSymbol mod symbol = explain LookupFailure (Map.lookup symbol mod >>= fromDynamic)

-- Types -----------------------------------------------------------------------

-- |
data Plugin a = Plugin { run :: a } deriving Show

-- |
data PluginError = LoadFailure | LookupFailure deriving Show

-- |
type DynamicModule = Map String Dynamic

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
