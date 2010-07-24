{-# LANGUAGE FlexibleContexts #-}
module PC.Compiler where

import Control.Monad.Reader hiding (liftIO)
import qualified Control.Monad.Reader as CMR
import MonadUtils hiding (liftIO)
import qualified MonadUtils as MU
import Exception

import GHC
import Name
import GHC.Paths (libdir) 		-- this is a Cabal thing
import DynFlags (defaultDynFlags)	-- this is a -package ghc thing
import Outputable (showSDoc, ppr)
import Digraph (flattenSCCs)

import PC.Config

compile :: Config -> IO ()
compile cfg = runReaderT compile' cfg

on_source :: MonadReader Config m => (Int -> Int -> FilePath -> m ()) -> m ()
on_source f =
     do cfg <- ask
	sequence_ $ zipWith (f $ length $ source cfg) [1 .. ] (source cfg)

compile' :: ReaderT Config IO ()
compile' =
     do on_source compileFile

compileFile :: Int -> Int -> FilePath -> ReaderT Config IO ()
compileFile total n targetFile =
     do CMR.liftIO $ putStrLn $
		"[" ++ show n ++ " of " ++ show total ++ "] Compiling " ++
		show targetFile ++ "..."
	CMR.liftIO $
		defaultErrorHandler defaultDynFlags $
		    runGhcT (Just libdir) (doCompileFile targetFile)
	CMR.liftIO $ putStrLn "  ...done"

doCompileFile :: FilePath -> GhcT IO ()
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
	parsed <- parseModule targetMod
	typed  <- typecheckModule parsed
	loadModule typed
	-- Figure out the top level definitions for the target
	maybeTargetModInfo  <- getModuleInfo (ms_mod targetMod)
	case maybeTargetModInfo of
	  Nothing -> MU.liftIO $ putStrLn "could not get moduleInfo"
	  Just targetModInfo -> compileModule targetName targetMod targetModInfo

compileModule name mod modInfo =
     do p <- parseModule mod
	let loc = getLoc $ pm_parsed_source p
	MU.liftIO  $ do putStrLn $ (showSDoc $ ppr name)
			putStrLn "\ntop level:"
			mapM_ processTopLev (modInfoTyThings modInfo)
			putStrLn "\nexports:"
			mapM_ processExport (modInfoExports modInfo)
			putStrLn "\nsrcloc:"
			putStrLn $ showSDoc $ pprDefnLoc loc
  where processTopLev ty =
	     do putStrLn $ (showSDoc $ ppr ty) ++ " at " ++
			   (showSDoc $ pprDefnLoc $ nameSrcSpan $ getName ty)
	processExport e =
	     do putStrLn $ (showSDoc $ ppr e) ++ " at " ++
			   (showSDoc $ pprDefnLoc $ nameSrcSpan e)

