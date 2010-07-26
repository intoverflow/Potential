module PC.Compiler (compiler) where

import Control.Monad
import Control.Monad.Reader (ask)

import Language.Haskell.Interpreter hiding (get)
import Language.Haskell.Interpreter.Unsafe

import System.Directory (createDirectoryIfMissing)
import System.IO (withFile, IOMode(..), hPutStrLn)
import System.FilePath (combine)

import qualified GHC as GHC

import PC.Config
import PC.Base
import PC.Source

import Potential.Printing

compiler :: Mode -> IO ()
compiler cfg =
     do let compilerState = CompilerState { msgDepth = 0 }
	putStrLn $ "Configuration: " ++ show cfg ++ "\n"
	(r, _) <- runCompiler (runInterpreter compiler) cfg compilerState
	case r of
	  Left err  -> printInterpreterError err
	  Right () -> putStrLn "Done compiling"
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
		mapM_ (\inFile ->
			     do fns <- inDepth $ analyzeFile inFile
				case cfg of
				  Check{}   -> doCheck fns
				  Compile{} -> doCompile inFile
							 (outDir cfg)
							 (if (outFile cfg == "")
							    then inFile ++ ".S"
							    else outFile cfg)
							 fns)
		      (inFiles cfg)

doCheck fns =
     do say "Functions:"
	inDepth $ mapM_ (\f -> say $ fname f ++ " defined at " ++ floc f) fns

doCompile mod outdir outfile fns =
     do let outname = combine outdir outfile
	say $ "Rendering to: `" ++ outname ++ "'"
	liftIO $ createDirectoryIfMissing True outdir
	liftIO $ withFile outname WriteMode $ \h ->
		     do hPutStrLn h $ "/* Generated by the Potential Compiler"
			hPutStrLn h $ " * 2010 http://potential-lang.org"
			hPutStrLn h $ " *"
			hPutStrLn h $ " * Initial source module: `" ++ mod ++"'"
			hPutStrLn h $ " */"
			mapM_ (doCompileFile h) fns

doCompileFile h fn =
     do putStrLn $ "  " ++ fname fn ++ "..."
	let indent = replicate 4 ' '
	hPutStrLn h $ ".globl " ++ fname fn
	hPutStrLn h $ fname fn ++ ":"
	hPutStrLn h $ indent ++ "// Defined at " ++ floc fn
	unless (null $ fnotes fn) $
	     do hPutStrLn h $ indent ++ "//"
		hPutStrLn h $ indent ++ "// Notes: " ++ show (fnotes fn)
		hPutStrLn h $ indent ++ "//"
	mapM_ (\c -> hPutStrLn h $ indent ++ show c) (fcode fn)
	hPutStrLn h ""

