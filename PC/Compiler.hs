{-# LANGUAGE FlexibleContexts #-}
module PC.Compiler where

import Control.Monad.Reader
import GHC
import Name
import GHC.Paths (libdir) 		-- this is a Cabal thing
import DynFlags (defaultDynFlags)	-- this is a -package ghc thing
import Outputable (showSDoc, ppr)
import Digraph (flattenSCCs)

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
			  runGhc (Just libdir) (doCompileFile targetFile)
	case res of
	  Nothing   -> liftIO $ putStrLn "Error: Nothing returned"
	  Just (nm, exps) -> liftIO $
		     do putStrLn $ (showSDoc $ ppr nm) ++ ":"
			mapM_ processExport exps
  where processExport e =
	     do let definedName = getOccString e
		    formalName  = showSDoc $ ppr e
		    loc         = showSDoc $ ppr $ getSrcLoc e
		putStrLn $ "  " ++ definedName ++
			   ": (" ++ formalName ++ ":" ++ loc ++ ")"
					 

doCompileFile targetFile =
     do dflags <- getSessionDynFlags
	setSessionDynFlags (dflags{ ctxtStkDepth = 160
				  , objectDir = Just "temp"
				  , hiDir = Just "temp" })
	target <- guessTarget targetFile Nothing
	setTargets [target]
	-- Dependency analysis
	-- TODO: make sure client code isn't cheating by importing forbidden
	-- fruit.
	modGraph <- do mg <- depanal [] False
		       return $ flattenSCCs $
				topSortModuleGraph False mg Nothing
	let targetMod  = last modGraph
	    targetName = ms_mod_name targetMod
	-- Load the code
	load LoadAllTargets
	-- Figure out the top level definitions for the target
	maybeTargetModInfo  <- getModuleInfo (ms_mod targetMod)
	case maybeTargetModInfo of
	  Nothing -> failNoModInfo
	  Just targetModInfo -> compileMod targetName
					   (modInfoExports targetModInfo)
  where failNoModInfo = return Nothing

compileMod targetName targetExports =
	return $ Just ( targetName , targetExports )

