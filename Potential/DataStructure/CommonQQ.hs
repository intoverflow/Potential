module Potential.DataStructure.CommonQQ where

import Data.Generics.Aliases (extQ)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote (dataToExpQ, dataToPatQ)

import Language.Haskell.TH.Lift (deriveLift)

import Potential.DataStructure.AbstractSyntax
import Potential.DataStructure.CodeGenerator

antiE :: UserStruct -> Maybe TH.ExpQ
antiE us = Just $ reifyStruct us
{-
     do defs <- reifyStruct us
	let s = show defs
	TH.litE $ TH.stringL s
-}

parseStructExp parser s =
     do loc <- TH.location
	let fname = TH.loc_filename loc
	    (line, col) = TH.loc_start loc
	parsed <- parser fname line col s
	dataToExpQ (const Nothing `extQ` antiE) parsed

parseStructPat parser s =
     do loc <- TH.location
	let fname = TH.loc_filename loc
	    (line, col) = TH.loc_start loc
	parsed <- parser fname line col s
	dataToPatQ (const Nothing) parsed


