{-
  Copyright 2010 Timothy Carstens    carstens@math.utah.edu

  This file is part of the Potential Standard Library.

    The Potential Standard Library is free software: you can redistribute it
    and/or modify it under the terms of the GNU Lesser General Public License as
    published by the Free Software Foundation, version 3 of the License.

    The Potential Standard Library is distributed in the hope that it will be
    useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with the Potential Standard Library.  If not, see
    <http://www.gnu.org/licenses/>.
-}
{-# LANGUAGE
	FunctionalDependencies,
	MultiParamTypeClasses,
	FlexibleInstances,
	FlexibleContexts,
	UndecidableInstances #-}
module Language.Potential.DataStructure.MetaData where

import Prelude (Integer, undefined, (++))
import Data.Word (Word64(..))

import Language.Potential.Size

class IsFieldOf typ field_label field_type
  | field_label -> typ
  , typ field_label -> field_type
 where
  forgetMask  :: typ -> field_label -> [Word64]
  isolateMask :: typ -> field_label -> [Word64]
  bitOffset   :: typ -> field_label -> [Integer]
  projField   :: typ -> field_label -> field_type
  projField _ _ = undefined
  injField    :: (IsFieldOf typ' field_label field_type')
			=> typ' -> field_label -> field_typ -> typ
  injField _ _ _ = undefined

class NumConstructors typ c | typ -> c

class AllConstructorsField typ field_label

data SubField a b = SubField a b

instance (IsFieldOf typ1 f1 typ2, IsFieldOf typ2 f2 typ3) =>
 IsFieldOf typ1 (SubField f1 f2) typ3 where
  forgetMask typ1 (SubField f1 f2) = (forgetMask typ1 f1) ++
					(forgetMask (projField typ1 f1) f2)
  isolateMask typ1 (SubField f1 f2) = (isolateMask typ1 f1) ++
					(isolateMask (projField typ1 f1) f2)
  bitOffset typ1 (SubField f1 f2) = (bitOffset typ1 f1) ++
					(bitOffset (projField typ1 f1) f2)


-- |Used to describe a sub-field.  Only valid when the parent has exactly one
-- constructor, since this provides no way to determine which constructor has
-- been used.
(-->) :: ( IsFieldOf typ1 f1 typ2
	 , IsFieldOf typ2 f2 typ3
	 , NumConstructors typ1 D1)
		=> f1 -> f2 -> SubField f1 f2
a --> b = SubField a b

