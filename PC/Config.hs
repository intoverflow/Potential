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

