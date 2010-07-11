module Potential.Array.CommonQQ
	( parseArrayExp
	, parseArrayPat
	) where

import Data.Generics.Aliases (extQ)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote (dataToExpQ, dataToPatQ)

import Potential.Array.AbstractSyntax
import Potential.Array.CodeGenerator

antiE :: UserArray -> Maybe TH.ExpQ
antiE us = Just $ reifyArray us

parseArrayExp parser s =
     do loc <- TH.location
	let fname = TH.loc_filename loc
	    (line, col) = TH.loc_start loc
	parsed <- parser fname line col s
	dataToExpQ (const Nothing `extQ` antiE) parsed

parseArrayPat parser s =
     do loc <- TH.location
	let fname = TH.loc_filename loc
	    (line, col) = TH.loc_start loc
	parsed <- parser fname line col s
	dataToPatQ (const Nothing) parsed



