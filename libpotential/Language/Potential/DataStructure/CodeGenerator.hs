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
{-# LANGUAGE TemplateHaskell #-}
module Language.Potential.DataStructure.CodeGenerator where

import Prelude
import qualified Language.Haskell.TH as TH

import Language.Potential.THLiftDecls
import Language.Potential.DataStructure.AbstractSyntax
import Language.Potential.Pointer
	( newPtr64 , primPtrProj, primPtrInj , primFieldProj, primFieldInj )


-- |Yields Template Haskell top-level declarations to define the given data
-- structure.
reifyStruct :: UserStruct -> TH.ExpQ
reifyStruct us =
     do decls <- mapM (\f -> f us)
			[ reifyType ]
	[| return $ concat decls |]


-- |Defines a (Haskell-level) data definition for the given user structure.
-- Includes a free variable for every field label; if a field label is used
-- by multiple constructors, each instance will have the same free variable.
reifyType :: UserStruct -> TH.Q [TH.Dec]
reifyType us =
     do let name = TH.mkName $ struct_name us
	    type_vars = map (TH.PlainTV . TH.mkName)
			    (varFieldNames $ allFields us)
	    non_strict n = (TH.NotStrict, TH.VarT $ TH.mkName n)
	    mkConstr c = let vars = map non_strict (varFieldNames $ fields c)
			 in TH.NormalC (TH.mkName $ constr_name c) vars
	return [TH.DataD [] name type_vars (map mkConstr $ constructors us) []]

