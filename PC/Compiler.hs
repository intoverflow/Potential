{-# LANGUAGE FlexibleContexts #-}
module PC.Compiler where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans
import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Unsafe

import qualified GHC as GHC
import qualified Outputable as Out

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
	unsafeSetGhcOption "-fcontext-stack=160"
	-- load the modules
	loadModules [targetFile]
	loaded <- getLoadedModules
	-- grab the target module
	let mod = head loaded
	say $ "Primary module: " ++ mod
	-- load into scope
	setTopLevelModules [mod]
	setImportsQ [("Potential.Assembly", Just "Potential.Assembly")]
	-- analyze the module
	exports <- getModuleExports mod
	mapM_ analyzeExport exports

analyzeExport export =
     do isFn <- typeChecks $ "Potential.Assembly.isFn " ++ (name export)
	when isFn $
	     do let strname = name export
		strname' <- interpret ("Potential.Assembly.funName " ++ strname)
				      (as :: String)
		say $ strname
		when (strname /= strname') $
		     say $ "  Warning: names do not match.  " ++
			   strname ++ " /= " ++ strname'
		typ <- typeOf strname
		-- say $ "  type: " ++ typ
		loc <- getExportLoc export
		say $ "  loc: " ++ loc
		

getExportLoc export = runGhc $
     do (n:_) <- GHC.parseName (name export)
	let loc = GHC.nameSrcSpan n
	return $ Out.showSDoc $ Out.ppr loc

