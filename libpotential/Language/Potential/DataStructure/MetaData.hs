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
{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}
module Language.Potential.DataStructure.MetaData where

import Prelude (Integer)
import Data.Word (Word64(..))

class IsFieldOf typ field_label field_type
  | field_label -> typ
  , typ field_label -> field_type
 where
  forgetMask  :: typ -> field_label -> Word64
  isolateMask :: typ -> field_label -> Word64
  bitOffset   :: typ -> field_label -> Integer
  projField   :: typ -> field_label -> field_type
  injField    :: (IsFieldOf typ' field_label field_type')
			=> typ' -> field_label -> field_typ -> typ

class NumConstructors typ c | typ -> c

