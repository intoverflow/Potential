module PC.Compiler (compiler) where

import Control.Monad
import Control.Monad.Reader (ask)
import Data.List (find)

import Language.Haskell.Interpreter hiding (get)
import Language.Haskell.Interpreter.Unsafe

import System.Directory (createDirectoryIfMissing)
import System.IO (withFile, IOMode(..), hPutStrLn)
import System.FilePath (combine)

import qualified GHC as GHC

import PC.Config
import PC.Base
import PC.Source

import Language.Potential.Printing

compiler :: Mode -> IO ()
compiler cfg =
     do let compilerState = CompilerState { msgDepth = 0 }
	putStrLn $ "Configuration: " ++ show cfg ++ "\n"
	(r, _) <- runCompiler (runInterpreter compiler) cfg compilerState
	case r of
	  Left err  ->
	     do putStrLn $ "Error during compile:"
		case err of
		  WontCompile errs -> mapM_ printInterpreterError errs
		  _ -> putStrLn $ show err
	  Right () -> putStrLn "Done compiling"
  where printInterpreterError (GhcError errMsg) = putStrLn errMsg
	compiler =
	     do -- set our flags
		unsafeSetGhcOption "-fcontext-stack=160"
		set [ languageExtensions :=
			[ NoImplicitPrelude
			, NoMonomorphismRestriction
			, QuasiQuotes
			, FlexibleContexts
			, FlexibleInstances
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
							 fns
				  GetType{} -> doGetTyp inFile
							(function cfg)
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
			hPutStrLn h $ ""
			hPutStrLn h $ ""
			mapM_ (doCompileFile h) fns

doCompileFile h fn =
     do putStrLn $ "  " ++ fname fn ++ "..."
	let indent = replicate 4 ' '
	hPutStrLn h $ "/* Function `" ++ fname fn ++ "' with type"
	hPutStrLn h (ftyp fn)
	hPutStrLn h "*/"
	hPutStrLn h $ ".globl " ++ fname fn
	hPutStrLn h $ fname fn ++ ":"
	hPutStrLn h $ indent ++ "// Defined at " ++ floc fn
	unless (null $ fnotes fn) $
	     do hPutStrLn h $ indent ++ "//"
		hPutStrLn h $ indent ++ "// Notes: " ++ show (fnotes fn)
		hPutStrLn h $ indent ++ "//"
	mapM_ (\c -> hPutStrLn h $ indent ++ show c) (fcode fn)
	hPutStrLn h ""

doGetTyp inFile func fns =
     if func == ""
	then mapM_ doGetTypOf fns
	else let mf = find (\f -> func == fname f) fns
	     in case mf of
		  Nothing -> say $ "Function `" ++ func ++
					"' not found in this module."
		  Just f -> doGetTypOf f

doGetTypOf f =
     do say $ fname f ++ ":"
	say $ ftyp f
	say ""

