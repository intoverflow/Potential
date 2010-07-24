{-# LANGUAGE FlexibleContexts #-}
module PC.Compiler where

import Data.Maybe (fromJust)
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
	mapM_ (\mod -> do let name_str = moduleNameString $ ms_mod_name mod
			  MU.liftIO $ putStrLn $ "..." ++ name_str
			  parseModule mod >>= typecheckModule >>= loadModule)
	      modGraph
	setContext (map ms_mod modGraph) [ms_mod targetMod]
	-- Figure out the top level definitions for the target
	maybeTargetModInfo  <- getModuleInfo (ms_mod targetMod)
	case maybeTargetModInfo of
	  Nothing -> MU.liftIO $ putStrLn "could not get moduleInfo"
	  Just targetModInfo -> compileModule targetName targetMod targetModInfo

compileModule name mod modInfo =
     do p <- parseModule mod
	MU.liftIO $ putStrLn $ (showSDoc $ ppr name)
	MU.liftIO $ putStrLn "\ntop level:"
	mapM_ processTopLev (modInfoTyThings modInfo)
  where processTopLev ty =
	     do let name_str = showSDoc $ ppr $ getName ty
		    loc_str  = showSDoc $ pprDefnLoc $ nameSrcSpan $ getName ty
		MU.liftIO $ putStrLn $ name_str
		MU.liftIO $ putStrLn $ "  " ++ loc_str
		gcatch (do typ <- exprType name_str
			   let typ_str = showSDoc $ ppr typ
			   MU.liftIO $ putStrLn $ "  Type " ++ typ_str)
		       eCatcher

eCatcher :: GhcException -> GhcT IO ()
eCatcher e = MU.liftIO $ putStrLn $ show e

