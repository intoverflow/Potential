{-# LANGUAGE
	FlexibleContexts,
	FlexibleInstances,
	MultiParamTypeClasses #-}
module PC.Compiler where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Language.Haskell.Interpreter hiding (get)
import Language.Haskell.Interpreter.Unsafe

import qualified GHC as GHC
import qualified Outputable as Out

import PC.Config

import Potential.Assembly (Instr)
import Potential.Printing

data CompilerState =
    CompilerState { onTarget :: Int
		  , totalTargets :: Int
		  , msgDepth :: Int }

instance MonadState CompilerState (InterpreterT Compiler) where
  get = lift get
  put s = lift (put s)

type Compiler = StateT CompilerState (ReaderT Config IO)

inDepth :: MonadState CompilerState m => m a -> m a
inDepth act =
     do let d = 2
	modify (\cs -> cs{ msgDepth = msgDepth cs + d })
	a <- act
	modify (\cs -> cs{ msgDepth = msgDepth cs - d })
	return a

compile :: Config -> IO ()
compile cfg =
     do let compilerState = CompilerState { onTarget = 0
					  , totalTargets = length $ source cfg
					  , msgDepth = 0 }
	putStrLn $ "Configuration: " ++ show cfg ++ "\n"
	runReaderT (runStateT (on_source compileFile) compilerState) cfg
	return ()

on_source :: (Target -> Compiler ()) -> Compiler ()
on_source f =
     do cfg <- ask
	mapM_ (inDepth . compileFile) (source cfg)

compileFile :: Target -> Compiler ()
compileFile target =
     do liftIO $ putStrLn $ "Compiling `" ++ show target ++ "'..."
	r <- runInterpreter (doCompileFile $ path target)
	case r of
	  Left err -> liftIO $ printInterpreterError err
	  Right () -> liftIO $ putStrLn "...done"

printInterpreterError :: InterpreterError -> IO ()
printInterpreterError e = putStrLn $ "Ups... " ++ (show e)

say :: String -> InterpreterT Compiler ()
say s =
     do depth <- (lift get) >>= return . msgDepth
	let ws = replicate depth ' '
	liftIO $ putStrLn $ ws ++ s

doCompileFile :: FilePath -> InterpreterT Compiler ()
doCompileFile targetFile = inDepth $
     do -- set our flags
	unsafeSetGhcOption "-fcontext-stack=160"
	set [ languageExtensions :=
		[ NoImplicitPrelude
		, NoMonomorphismRestriction
		, QuasiQuotes
		, FlexibleContexts
		, MultiParamTypeClasses
		, UndecidableInstances
		, TypeFamilies
		] ]
	-- load the modules
	loadModules [targetFile]
	loaded <- getLoadedModules
	-- grab the target module
	let mod = head loaded
	-- load into scope
	setTopLevelModules [mod]
	setImportsQ [ ("Potential", Just "Potential")
		    , ("Potential.Assembly", Just "Potential.Assembly") ]
	-- analyze the module
	say $ "Module " ++ mod ++ " defines functions:"
	exports <- getModuleExports mod
	mapM_ (inDepth . analyzeExport) exports
	reset

analyzeExport export =
     do isFn <- typeChecks $ "Potential.isFn " ++ (name export)
	when isFn $
	     do let strname = name export
		strname' <- interpret ("Potential.funName " ++ strname)
				      (as :: String)
		say $ strname' ++ ":"
		inDepth $ inDepth $
		     do when (strname /= strname') $
			     say $ "// Warning: names do not match.  " ++
				   strname ++ " /= " ++ strname'
			typ <- typeOf strname
			-- say $ "  type: " ++ typ
			loc <- getExportLoc export
			say $ "// Defined at: " ++ loc
			code <- interpret ("Potential.getAssembly " ++ strname)
					  (as :: [Instr])
			mapM_ (say . show) code

getExportLoc export = runGhc $
     do (n:_) <- GHC.parseName (name export)
	let loc = GHC.nameSrcSpan n
	return $ Out.showSDoc $ Out.ppr loc

