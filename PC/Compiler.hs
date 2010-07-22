{-# LANGUAGE FlexibleContexts #-}
module PC.Compiler where

import Control.Monad.Reader
import GHC
import GHC.Paths (libdir) -- this is a Cabal thing
import DynFlags (defaultDynFlags)	-- this is a -package ghc thing

import Outputable

import PC.Config

compile :: MonadIO m => Config -> m ()
compile cfg = runReaderT compile' cfg

on_source :: MonadReader Config m => (Int -> Int -> FilePath -> m ()) -> m ()
on_source f =
     do cfg <- ask
	sequence_ $ zipWith (f $ length $ source cfg) [1 .. ] (source cfg)

compile' :: MonadIO m => ReaderT Config m ()
compile' =
     do on_source compileFile

compileFile :: MonadIO m => Int -> Int -> FilePath -> ReaderT Config m ()
compileFile total n targetFile =
     do liftIO $ putStrLn $
		"[" ++ show n ++ " of " ++ show total ++ "] Compiling " ++
		show targetFile ++ "..."
	res <- liftIO $ defaultErrorHandler defaultDynFlags $
	     do runGhc (Just libdir) $
		     do dflags <- getSessionDynFlags
			setSessionDynFlags (dflags{ ctxtStkDepth = 160 })
			target <- guessTarget targetFile Nothing
			setTargets [target]
			load LoadAllTargets
			modSum <- getModSummary $ mkModuleName "B"
			p <- parseModule modSum
			t <- typecheckModule p
			d <- desugarModule t
			l <- loadModule d
			n <- getNamesInScope
			c <- return $ coreModule d
			g <- getModuleGraph
			mapM showModule g     
			return $ (parsedSource d, "/n-----/n", typecheckedSource d)
	liftIO $ print $ showSDoc (ppr res)

