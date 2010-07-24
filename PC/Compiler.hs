{-# LANGUAGE FlexibleContexts #-}
module PC.Compiler where

import Control.Monad.Reader
import Control.Monad.Trans
import Language.Haskell.Interpreter

import qualified GHC as GHC

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
     do liftIO $ putStrLn $
		"[" ++ show n ++ " of " ++ show total ++ "] Compiling " ++
		show targetFile ++ "..."
	r <- runInterpreter (doCompileFile targetFile)
	case r of
	  Left err -> liftIO $ printInterpreterError err
	  Right () -> liftIO $ putStrLn "...done"

printInterpreterError :: InterpreterError -> IO ()
printInterpreterError e = putStrLn $ "Ups... " ++ (show e)

say :: String -> InterpreterT (ReaderT Config IO) ()
say = liftIO . putStrLn

doCompileFile :: FilePath -> InterpreterT (ReaderT Config IO) ()
doCompileFile targetFile =
     do -- set our flags
	runGhc $ do dflags <- GHC.getSessionDynFlags
		    GHC.setSessionDynFlags $
			  dflags{ GHC.ctxtStkDepth = 160
				, GHC.objectDir = Just "temp"
				, GHC.hiDir = Just "temp" }
	-- load the modules
	loadModules [targetFile]
	loaded <- getLoadedModules
	-- grab the target module
	let mod = head loaded
	say $ "Primary module: " ++ show mod
	interpd <- isModuleInterpreted mod
	if interpd
	  then say "...interpreted"
	  else say "...NOT interpreted"
	-- load into scope
	setTopLevelModules [mod]
	setImports []
	-- analyze the module
	exports <- getModuleExports mod
	mapM_ (say . show) exports

