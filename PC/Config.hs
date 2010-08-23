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
    along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DeriveDataTypeable #-}
module PC.Config where

import System.Environment
import System.Console.CmdArgs

import Control.Monad.State
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (fromJust)

data Mode =
    Compile { inFiles :: [String]
	    , outDir  :: String
	    , outFile :: String }
  | Check { inFiles :: [String] }
  | GetType { inFiles :: [String]
	    , function :: String }
  deriving (Show, Data, Typeable)

compileMode = mode $
	Compile { inFiles = def &= text "Input file"
				& explicit
				& flag "infile"
				& typ "MODULE"
				& args
		, outDir = "asm" &= text "Output directory"
				& explicit
				& flag "outdir"
				& typDir
		, outFile = def &= text "Output file (rel. to output directory)"
				& explicit
				& flag "outfile"
				& typFile }
	&= defMode
checkMode = mode $
	Check { inFiles = def &= text "Input file"
				& explicit
				& flag "infile"
				& typ "MODULE"
				& args }
getTypeMode = mode $
	GetType { inFiles = def &= text "Input file"
				& explicit
				& flag "infile"
				& typ "MODULE"
				& args
		, function = def &= text "Function to inspect"
				& explicit
				& flag "function"
				& typ "IDENTIFIER" }

