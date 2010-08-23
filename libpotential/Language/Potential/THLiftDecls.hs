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
{-# LANGUAGE
	TemplateHaskell,
	TypeSynonymInstances #-}
module Language.Potential.THLiftDecls where

import Prelude
import Language.Haskell.TH
import qualified Language.Haskell.TH.Syntax as THS
-- Requires:
--    th-lift on Hackage
--    by: Mathieu Boespflug <mboes@tweag.net>
import Language.Haskell.TH.Lift (deriveLift)

deriveLift ''Dec
deriveLift ''Kind
deriveLift ''FamFlavour
deriveLift ''Pragma
deriveLift ''Foreign
deriveLift ''FunDep
deriveLift ''Con
deriveLift ''TyVarBndr
deriveLift ''Pred
deriveLift ''Body
deriveLift ''Pat
deriveLift ''Clause
deriveLift ''Type
deriveLift ''Lit
deriveLift ''Exp
deriveLift ''Guard
deriveLift ''Strict
deriveLift ''Safety
deriveLift ''Callconv
deriveLift ''InlineSpec
deriveLift ''Stmt
deriveLift ''Range
deriveLift ''Match

instance THS.Lift Rational where
  lift a = undefined

