{-
  Copyright 2010 Timothy Carstens    carstens@math.utah.edu

  This file is part of the Potential Compiler.

    The Potential Compiler is free software: you can redistribute it and/or 
    modify it under the terms of the GNU General Public License as published 
    by the Free Software Foundation, version 3 of the License.

    The Potential Compiler is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with the Potential Compiler.  If not, see
    <http://www.gnu.org/licenses/>.
-}
module PC.Source (analyzeFile, AssemblyCode(..), Warning(..)) where

import Control.Monad
import Language.Haskell.Interpreter hiding (get)

import qualified GHC as GHC
import qualified FastString as FS

import Language.Potential.Assembly (Instr)

import PC.Config
import PC.Base

data Warning = NameMismatch String String
  deriving Show

data AssemblyCode =
    AssemblyCode { fname :: String
		 , fnotes :: [Warning]
		 , ftyp  :: String
		 , floc  :: String
		 , fcode :: [Instr] }

analyzeFile :: String -> InterpreterT Compiler [AssemblyCode]
analyzeFile target =
     do liftIO $ putStrLn $ "Compiling `" ++ target ++ "'..."
	doAnalyzeFile target

doAnalyzeFile :: FilePath -> InterpreterT Compiler [AssemblyCode]
doAnalyzeFile targetFile =
     do -- load the modules
	loadModules [targetFile]
	loaded <- getLoadedModules
	-- grab the target module
	let mod = head loaded
	-- load into scope
	setTopLevelModules [mod]
	setImportsQ [ ("Language.Potential", Just "Language.Potential")
		    , ("Language.Potential.Assembly", Just "Language.Potential.Assembly") ]
	-- analyze the module
	-- say $ "Module " ++ mod ++ " defines functions:"
	exports <- getModuleExports mod
	fns <- mapM (inDepth . analyzeExport) exports
	return $ concat fns

analyzeExport export =
   do isFn <- typeChecks $ "Language.Potential.isFn " ++ (name export)
      if not isFn
	then return []
	else do let strname = name export
		strname' <- interpret ("Language.Potential.funName " ++ strname)
				      (as :: String)
		-- say $ strname' ++ ":"
		inDepth $ inDepth $
		     do let notes = if (strname /= strname')
					then [NameMismatch strname strname']
					else []
			typ <- typeOf strname
			-- say $ "  type: " ++ typ
			loc <- getExportLoc export
			-- say $ "// Defined at: " ++ loc
			code <- interpret ("Language.Potential.getAssembly " ++ strname)
					  (as :: [Instr])
			-- mapM_ (say . show) code
			return [ AssemblyCode { fname = strname'
					      , floc = loc
					      , ftyp = typ
					      , fnotes = notes
					      , fcode = code } ]

getExportLoc export = runGhc $
     do (n:_) <- GHC.parseName (name export)
	let loc = GHC.nameSrcSpan n
	    filename = FS.unpackFS $ GHC.srcSpanFile loc
	    fileline = show $ GHC.srcSpanStartLine loc
	return $ filename ++ ":" ++ fileline

