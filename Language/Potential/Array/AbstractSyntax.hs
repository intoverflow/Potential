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
	DeriveDataTypeable #-}
module Language.Potential.Array.AbstractSyntax where

import Prelude
import Data.Typeable
import Data.Data
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as THS

data UserArray =
      UserArray { array_name :: String
		, fields :: [Field]
		}
  deriving (Eq, Show, Data, Typeable)

instance THS.Lift UserArray where
  lift ua = foldl TH.appE [| UserArray |]
			[ THS.lift $ array_name ua
			, THS.lift $ fields ua
			]

data Field =
    VarField { field_name :: String
	     , field_pos :: Integer
	     }
  | ReservedField { field_pos :: Integer }
  deriving (Eq, Show, Data, Typeable)

instance THS.Lift Field where
  lift (VarField n p) = foldl TH.appE [| VarField |]
				      [ THS.lift n, THS.lift p ]
  lift (ReservedField p) = TH.appE [| ReservedField |] (THS.lift p)

isVarField :: Field -> Bool
isVarField (a@VarField{}) = True
isVarField _ = False

fillIn :: [Field] -> [Field]
fillIn [] = []
fillIn [a] = [a]
fillIn (a:b:fs) =
    let as = map ReservedField [ field_pos a + 1 .. field_pos b - 1 ]
    in [a] ++ as ++ fillIn (b:fs)

isStrictlyIncreasing :: [Field] -> Bool
isStrictlyIncreasing fs = isi fs
  where isi [] = True
	isi [a] = True
	isi (a:b:fs) | field_pos a < field_pos b = isi (b:fs)
		     | otherwise = False


