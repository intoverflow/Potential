module PC.Compiler (doCompile) where

import Control.Monad.Reader (ask)

import Language.Haskell.Interpreter hiding (get)
import Language.Haskell.Interpreter.Unsafe

import qualified GHC as GHC
import qualified Outputable as Out

import PC.Config
import PC.Base
import PC.Source

doCompile :: Config -> IO ()
doCompile cfg =
     do let compilerState = CompilerState { msgDepth = 0 }
	putStrLn $ "Configuration: " ++ show cfg ++ "\n"
	(r, _) <- runCompiler (runInterpreter compiler) cfg compilerState
	case r of
	  Left err -> liftIO $ printInterpreterError err
	  Right () -> liftIO $ putStrLn "Done compiling"
  where printInterpreterError err = putStrLn $ "Ups... " ++ (show err)
	compiler =
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
		-- one file at a time
		cfg <- lift ask
		inDepth $ compileFile (target cfg)

