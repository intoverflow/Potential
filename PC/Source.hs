module PC.Source (compileFile) where

import Control.Monad
import Language.Haskell.Interpreter hiding (get)

import qualified GHC as GHC
import qualified Outputable as Out

import Potential.Assembly (Instr)
import Potential.Printing

import PC.Config
import PC.Base

compileFile :: Target -> InterpreterT Compiler ()
compileFile target =
     do liftIO $ putStrLn $ "Compiling `" ++ show target ++ "'..."
	doCompileFile $ path target

doCompileFile :: FilePath -> InterpreterT Compiler ()
doCompileFile targetFile =
     do -- load the modules
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

