{-
  Copyright 2010 Timothy Carstens    carstens@math.utah.edu

  This file is part of the Potential Standard Library.

    The Potential Standard Library is free software: you can redistribute it
    and/or modify it under the terms of the GNU Lesser General Public License as
    published by the Free Software Foundation, version 3 of the License.

    The Potential Compiler is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
-}
module Language.Potential.DataStructure.CommonQQ where

import Prelude
import Data.Generics.Aliases (extQ)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote (dataToExpQ, dataToPatQ)

import Language.Potential.DataStructure.AbstractSyntax
import Language.Potential.DataStructure.CodeGenerator

antiE :: UserStruct -> Maybe TH.ExpQ
antiE us = Just $ reifyStruct us

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


